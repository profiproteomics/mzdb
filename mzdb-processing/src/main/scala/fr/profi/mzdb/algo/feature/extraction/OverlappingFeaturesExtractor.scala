/**
 *
 */
package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.OverlappingIsotopicPattern
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Feature
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import fr.profi.mzdb.model.TheoreticalIsotopePattern
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import scala.collection.mutable.ListBuffer
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.utils.ms.IsotopicPatternLookup
import fr.profi.mzdb.algo.feature.scoring.FeatureScorer
import fr.profi.mzdb.utils.ms.MsUtils

/**
 * 
 */
case class OverlappingExtractorParameters(//mzTolPPM: Float,
                                          minNbOverlappingIPs: Int = 1,  
                                          maxZ: Int = 6, 
                                          maxIpShift: Int = 3,
                                          minPeakelCorrToMono:Float = 0.75f,
                                          minIntensityToMono: Float = 0.6f)
                                          //maxConsecutiveGaps:Int = 2,
                                          //maxTimeWindow:Float = 500) //make it adaptive to the theoIP

/**
 * 
 */
case class OverlapStatus( overlapEvidence: Boolean,
                          foundPossibleMonoisotopic: Boolean,
                          possibleMonosisotopicPeakel: Pair[Option[Peakel], Option[Feature]], 
                          apexDeviation: Float)

case class minMonoisotopicThreshold ()

/**
 * StraitForward implementation
 * @author Marco
 * params: set as var, so parameters values can be changed after OverlappingFeaturesExtractor creation
 * inherit from Ms2DrivenExtractor essentially to fetch parameters
 */
class OverlappingFeaturesExtractor ( var params: OverlappingExtractorParameters,
                                    override val scanHeaderById: Map[Int,ScanHeader],
                                    override val nfByScanId: Map[Int,Float],
                                    override val mzTolPPM: Float,
                                    override val maxNbPeaksInIP: Int, // TODO: remove this param
                                    override val minNbOverlappingIPs: Int,
                                    override val maxConsecutiveGaps: Int = 2,
                                    override val maxTimeWindow: Float = 1200f,
                                    override val minPercentageOfMaxInt: Float = 0.005f) 
                                    
        extends Ms2DrivenFtExtractor (scanHeaderById: Map[Int,ScanHeader],
                                      nfByScanId: Map[Int,Float],
                                      mzTolPPM: Float,
                                      maxNbPeaksInIP: Int, // TODO: remove this param
                                      minNbOverlappingIPs: Int,
                                      maxConsecutiveGaps: Int,
                                      maxTimeWindow: Float,
                                      minPercentageOfMaxInt: Float) {
  
  /********************************************************************
   * UTILITY FUNCTIONS
   ********************************************************************/
  
  /**
   * specialized implementation
   */
  protected def _getIntensityAscendantDirection(  mzToExtract:Double,
                                                  pklTree: PeakListTree,
                                                  cycleNum: Int,
                                                  range: Pair[Int, Int]): Int = {
     var (firstCycleNum, lastCycleNum) = (0, 0)

    // Left check
    firstCycleNum = cycleNum - range._2
    lastCycleNum = cycleNum - range._1
    val leftIntSum = this._integrateIntensity( mzToExtract, pklTree, firstCycleNum, lastCycleNum)

    // Right check
    firstCycleNum = cycleNum + range._1
    lastCycleNum = cycleNum + range._2
    val rightIntSum = this._integrateIntensity(mzToExtract, pklTree, firstCycleNum, lastCycleNum )

    // Determine the direction by comparing the summed intensities
    var ascDirection = 0
    if (leftIntSum >= 0 || rightIntSum >= 0) {
      if (rightIntSum > leftIntSum) {
        ascDirection = 1
      } else {
        ascDirection = -1
      }
    }
    ascDirection
  }
  
  /**
   * specialized implementation
   */
  protected def _integrateIntensity(mzToExtract:Double,
                                    pklTree: PeakListTree,
                                    firstCycle: Int, lastCycle: Int): Double = {

    var intensitySum = 0.0

    // Sum the forward isotopic profile intensities
    breakable {
      for (curCycleNum <- firstCycle to lastCycle) {

        if (this.ms1ScanIdByCycleNum.contains(curCycleNum) == false) break

        val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
        val curScanH = this.scanHeaderById(curScanId)

        val ip = pklTree.getNearestPeak(curScanId, mzToExtract, mzToExtract * this.mzTolPPM / 1e6)

        if (ip.isDefined) {
          intensitySum += ip.get.getIntensity()
        }
      }
    }

    intensitySum
  }
    
  
  /**
   * 
   */
  protected def _extractXIC( mzToExtract: Double,
                             //putativeFt: PutativeFeature,
                             pklTree: PeakListTree,
                             startingScanHeader: ScanHeader ): Array[Peak] = {
    
    val peaks = new ArrayBuffer[Peak]()
    
    val cycleNum = startingScanHeader.getCycle
    var apexTime = startingScanHeader.getTime
    
    //val theoIP = putativeFt.theoreticalIP
    
    // Determine intensity ascendant direction
    val range = Pair(1,15)
    var ascDirection = this._getIntensityAscendantDirection( mzToExtract, 
                                                             pklTree,
                                                             cycleNum, 
                                                             range)
    // 1 => right
    // -1 => left
    if( ascDirection == 0 ) // if no signal found on both sides
      return peaks.toArray
    
    // --- Extract isotopic patterns --- //
    var curMaxIntensity = 0.0
    var lastIntensity = 0f
    
    // Iterate until left and right directions have been analyzed
    var numOfAnalyzedDirections = 0
        
    while( numOfAnalyzedDirections < 2 ) {
      
      var timeOverRange = false
      var consecutiveGapCount = 0
      var cycleShift = 0
      
      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      breakable {
        while( consecutiveGapCount <= this.maxConsecutiveGaps && !timeOverRange ) {
          
          // Decrease cycle shift if LEFT direction
          if( ascDirection == -1 ) {
            cycleShift -= 1 
          }
          
          // Determine current cycle number
          val curCycleNum = cycleNum + cycleShift
          
          // Try to retrieve the scan id
          var curScanHOpt = Option.empty[ScanHeader]
          if( this.ms1ScanIdByCycleNum.contains(curCycleNum) ) {
            // Retrieve the wanted scan header
            curScanHOpt = this.scanHeaderById.get(this.ms1ScanIdByCycleNum(curCycleNum))
          }
          
          if( curScanHOpt.isEmpty ) {//if wrong scanID
            timeOverRange = true
            break
          } else {
            val curScanH = curScanHOpt.get
            val curTime = curScanH.getTime
              
            // check if total time does not exceed the provided threshold
            if( this.maxTimeWindow > 0 && math.abs(curTime - apexTime) > this.maxTimeWindow / 2 ) {
              timeOverRange = true
              break
            }
            
            //get peaks here IMPORTANT
            val curScanId = curScanH.getId
            val ipOpt = pklTree.getNearestPeak(curScanId, mzToExtract, mzToExtract * this.mzTolPPM / 1e6 ) //the last need tol in dalton
            //val ipOpt = pklTree.extractIsotopicPattern( curScanH, theoIP, mzTolPPM, 2 )
            
            //WEIRD CASES
            if( cycleShift == 0 && ipOpt.isEmpty ) {
              // Sometimes the precursor m/z is wrong => just skip these weird cases
              //this.logger.trace( "supervised ft extraction failed at scan id=%06d & mz=%f".format(curScanH.getId, putativeFt.getMz) )
            }
            
            // Check if an isotopic pattern has been found
            if( ipOpt.isDefined ) {
              
              val ip = ipOpt.get
              val intensity = ip.getIntensity()
              
              // Add the isotopic pattern to the list of extracted IPs
              if( ascDirection == 1 )
                peaks += ip // append IP
              else if( ascDirection == -1 )
                peaks.+=:(ip) // prepend IP
                
              // Analysis of the isotopic pattern intensity
              if( intensity > curMaxIntensity ) {
                // Update information about the apex
                curMaxIntensity = intensity
                apexTime = curTime
              }
                
                //lastIntensity = intensity
              //}
              
              // test intensity < intensityThreshold
              if( intensity == 0 || intensity < curMaxIntensity * this.minPercentageOfMaxInt )
                consecutiveGapCount += 1
              else 
                consecutiveGapCount = 0
            
            } else { //ip not defined
              consecutiveGapCount += 1
            }
  
            // Increase cycle shift if right direction
            if( ascDirection == 1 ) 
              cycleShift += 1
            
          }// END OF ELSE
        } // END OF WHILE
      }
      ascDirection *= -1
      numOfAnalyzedDirections += 1
    }
    
    peaks.toArray
  }
  
  /************************************************************************************************
   * EXTRACTION OVERLAPPING FEATURES, FIRST METHOD
   ************************************************************************************************/
  /**
   * classical 
   */
  protected def _extractOverlappingIps(ip: IsotopicPattern, 
                                       theoIP: TheoreticalIsotopePattern, 
                                       pklTree: PeakListTree): Array[OverlappingIsotopicPattern] = {
    
    // Unpack parameters
    val maxZ = this.params.maxZ
    val maxIpShift = this.params.maxIpShift
    val mzTolPPM = this.mzTolPPM
    
    // Some require statements
    require(maxZ > 0, "maximum charge must be strictly positive")
    require(maxIpShift > 0, "maximum IP shift must be strictly positive")

    // Search for overlapping isotopic patterns
    val olpIPs = new ArrayBuffer[OverlappingIsotopicPattern]()

    // Try several charge states
    for (z <- 1 to maxZ) {

      // Try several m/z shifts
      for (ipShift <- (-maxIpShift) until 0) {

        val olpIpMz = ip.mz + (ipShift.toDouble / z)
        val olpIpNbPeaksToReachMonoistopicPeakel = ipShift + 1
        
        // Configure a new theoretical isotope pattern
        val tmpTheoIP = theoIP.copy(
            mz = olpIpMz,
            charge = z,
            relativeAbundances = theoIP.relativeAbundances.take(olpIpNbPeaksToReachMonoistopicPeakel)
        )

        // Try to extract a putative overlapping isotopic pattern
        val tmpOlpIp = pklTree.extractOverlappingIsotopicPattern(
          scanHeader = ip.scanHeader,
          theoreticalIP = tmpTheoIP,
          mzTolPPM = mzTolPPM,
          nbPeaksToSum = 2,
          overlapShift = ipShift
        )

        // Check that we retrieved enough peaks
        if (tmpOlpIp.isDefined && olpIpNbPeaksToReachMonoistopicPeakel <= tmpOlpIp.get.peaks.length) {
          // Set overlapping IP elution time
          //tmpOlpIp.elutionTime = ip.getElutionTime;
          olpIPs += tmpOlpIp.get
        }
      }//end inner for
    } //outerfor
    
    olpIPs.toArray
  }
  
   /**
   * use in conjugation with _extractOverlappingIps
   */
  def _buildOverlappingFeatures( feature: Feature ): Array[Feature] = { // minNbIPs: Int = 3

    // Check that the current features has peak
    if (feature.peakelsCount == 0)
      return null
    
    val minNbOverlappingIPs = this.params.minNbOverlappingIPs
    val maxConsecutiveGapInEachPeakels = 1
      
    val isotopicPatterns = feature.getIsotopicPatterns
      
    // Group overlapping isotopic patterns by charge state and overlapShift of peaks
    //Perform three successive grouping: 1 charge, 2 ovlShift,( 3 nbPeaks )
    val ovlIpsByCharge = new HashMap[Int, ArrayBuffer[OverlappingIsotopicPattern]]()
    
    //group ovlIps by charge
    for (ip <- isotopicPatterns) if (ip.overlappingIps != null) {
      for (ovlIp <- ip.overlappingIps) {
        val ovlIpZ = ovlIp.charge
         val sameChargedOvlIps = ovlIpsByCharge.getOrElseUpdate(ovlIpZ, new ArrayBuffer[OverlappingIsotopicPattern])
         sameChargedOvlIps += ovlIp
      }
    }
    
    //goup ovlIps by ipShift and build Feature object
    val ovlFts = new ArrayBuffer[Feature]
    ovlIpsByCharge.foreach{ case (charge, ovlIps) =>
       val ovlIpsByShift = ovlIps.groupBy(_.overlapShift)
       
       ovlIpsByShift.foreach{ case (shift, ovlIps) =>
         //test if we find two ovlIps corresponding to the same ScanId, euh ? not possible
         
         if ( ovlIps.length >= params.minNbOverlappingIPs ) {
           val ovlFt = new Feature(ovlIps(0).mz, charge, ovlIps)
           ovlFt.mz = ovlFt.peakels.head.getApex.getMz()
           ovlFts += ovlFt
         }
       }
    }
    
    //ensure that all peakels have less than maxConsecutiveGapInEachPeakels
    val finalOvlFts = new ArrayBuffer[Feature]
    ovlFts.foreach{ ovlFt =>
      var broke = false
      breakable {
        ovlFt.peakels.foreach { peakel =>
          val peaks = peakel.peaks
          var gapCount = 0
          for (i  <- 0 until peaks.length) {
            if (peaks(i).isDefined)
              gapCount = 0
            else {
              gapCount += 1
              if (gapCount > maxConsecutiveGapInEachPeakels) {
                 broke = true
                 break
              }
            }
          }//end inner for
        }
      }//end breakable
      if (! broke)
        finalOvlFts += ovlFt
    }
       
    finalOvlFts.toArray
  }
  
  /************************************************************************************************
   * EXTRACTION OVERLAPPING FEATURES, SECOND METHOD
   ************************************************************************************************/
  
  /**
   * same as above but using wavelet
   * depnds on what we will do, but could be called: extractOverlappingPeakel
   */
  protected def  _extractOverlappingFeatures(ftFirstPeakel:Peakel, //Seq[IsotopicPattern],
                                             ftZ: Int,
                                             //theoIP: TheoreticalIsotopePattern, 
                                             pklTree: PeakListTree): Array[Feature] = {
    
    // Unpack parameters
    val maxZ = this.params.maxZ
    val maxIpShift = this.params.maxIpShift
    val mzTolPPM = this.mzTolPPM
    
    // Some require statements
    require(maxZ > 0, "maximum charge must be strictly positive")
    require(maxIpShift > 0, "maximum IP shift must be strictly positive")
    
    //getting the maxScanId
    //the idea is to use extractIsotopicPattern and use only first peakel
    val maxScanId = ftFirstPeakel.peaks.filter(x=> x.isDefined)
                                       .sortBy(x => x.get.getIntensity())
                                       .last.get
                                       .getLcContext().getScanId()
    
    // Search for overlapping isotopic patterns
    val olpIPs = new ArrayBuffer[OverlappingIsotopicPattern]()
    val peakelsByCharge = new HashMap[Int, Array[Peakel]]
    
    
    var mozAlreadyCheck = Set[Int]()
    // Try several charge states
    for (z <- 1 to maxZ) {
      val sameChargedPeakels = new ArrayBuffer[Peakel] 
      // Try several m/z shifts
      for (ipShift <- (-maxIpShift) until 2) { //enter only once into that boucle
        val mzToExtract = ftFirstPeakel.mz + (ipShift.toDouble / z)
        val mzToExtractId = math.round( (mzToExtract * 1000) toFloat)
        
        //Extract a the mass if not XIC not already computed
        val peaks = new ArrayBuffer[Peak]()
        if ( ! mozAlreadyCheck.contains(mzToExtractId) ) {
          peaks ++ this._extractXIC( mzToExtract, pklTree, this.scanHeaderById(maxScanId) )
          mozAlreadyCheck += mzToExtractId
        }
        
        if (! peaks.isEmpty) {
          val wpf = new WaveletBasedPeakelFinder(peaks)
          wpf.ridgeFilteringParams.minSNR = 1.5f
          val peakelsIndexes = wpf.findPeakelsIndexes
          if (! peakelsIndexes.isEmpty) {
            // TODO:We take the most intense, the closest ?
            val (minIdx, maxIdx) = (peakelsIndexes.head._1, math.min(peakelsIndexes.head._2, peaks.length))
            // here the first index in the constructor is the index of the peakel in the feature
            // we do not need it in our case, so set it to 0
            sameChargedPeakels += new Peakel( 0, peaks.slice(minIdx, maxIdx).map(Some(_) ) toArray ) 
          }
        }
      }//end ipShift for
      peakelsByCharge(z) = sameChargedPeakels.toArray
    }//end z for
    
    
    // got more confidence in the extract pattern of the pkltree
    //in order to avoid changing the model re-extract isotopicPattern objects
    peakelsByCharge.foreach{ case (charge, peakels) =>
      if (! peakels.isEmpty) {
        val farthestpeakel = peakels.sortBy(_.mz).head
        farthestpeakel.peaks.map { p =>
          if (p.isDefined) {
            val scanH = this.scanHeaderById(p.get.getLcContext().getScanId())
            pklTree.extractIsotopicPattern( scanH, IsotopicPatternLookup.getTheoreticalPattern(farthestpeakel.mz, charge), this.mzTolPPM, 2)
          } else {
              None
          }
        }
      }
    }
    
    null
  }

  
 /************************************************************************************************
   * OVERLAPPING STATUS EVALUATION
   ************************************************************************************************/
  
  /**
   *  return overlapping status
   *  look for overlapping feature with the same charge than the considered feature
   *  and check intensity and apex deviation of the considered peakel
   */
  def _evaluateOverlappingStatus( ft:Feature, ovlFeatures: Array[Feature] ) : OverlapStatus = {
    //simple case
    if (ovlFeatures.isEmpty) {
      return OverlapStatus(overlapEvidence = false, 
                          foundPossibleMonoisotopic = false,
                          possibleMonosisotopicPeakel= Pair(None, None),
                          apexDeviation =  0f)
    }
    //difficult cases
    //apply peak detection on the first peakel
    //could be optional, or using the basic peakel finder
    //build an inverse mapping
    
    val overlapEvidence = true
        
    //setting overlappingFeatures
    ft.overlappingFeatures = ovlFeatures
     
    //determining best overlappingFeature
    if (ovlFeatures.length == 1)
      ft.bestOverlappingFeature = ovlFeatures(0)
    else {
      //best overlapping calculation, HighestPMCC
      //TODO: take the most intense, the closestPeakel to the monoisotopic ?
      ft.bestOverlappingFeature = ovlFeatures(0)
      FeatureScorer.calcOverlappingFactor(ft, this.mzTolPPM)
    }
    
    // Determine if there is a possible monoIsotopic which would be good
    val featureByPeakels = new HashMap[Peakel, Feature]()
    ovlFeatures.foreach{ ovlFt => ovlFt.peakels.foreach( featureByPeakels(_) = ovlFt) } 
    
    //if (!ovlFt.peakels.isEmpty) {
    //  val peaks = ovlFt.peakels.head.definedPeaks
    //  val wpf = new WaveletBasedPeakelFinder(peaks)
    //  wpf.ridgeFilteringParams.minSNR = 1.2f
    //  if (wpf.findPeakelsIndexes().isEmpty)
    //    broke = true
    //}
    
    val allPeakels = ovlFeatures.map (_.peakels).flatten
    
    // we assume than if ft has several peakel, the most possible monoisotopic peakel
    // mass must correpond to a moz difference corresponding to the charge of the 
    // feature of interest
    if (ft.peakelsCount > 1) {
      val mozToCheck = ft.mz - (1.0027/ft.charge)    //TODO: grab the hydrogen mass from a static, enum definition
      val mzTolDa = MsUtils.ppmToDa(mozToCheck, this.mzTolPPM)
      val mzBounds = Pair(mozToCheck - mzTolDa, mozToCheck + mzTolDa)
      val monoisotopicCandidates = allPeakels.filter(peakel => peakel.mz > mzBounds._1 && peakel.mz < mzBounds._2 )
      
      if (monoisotopicCandidates.isEmpty) {
        return OverlapStatus(overlapEvidence = true, 
                            foundPossibleMonoisotopic = false,
                            possibleMonosisotopicPeakel= Pair(None, None),
                            apexDeviation =  0f)
      }
      
      //monoisotopic founds, which one the best ?
      //Tuple4
      // correlation, intensity, apexIndex,  
      val scoring = new HashMap[Peakel, Tuple3[Float, Float, Int]]
      monoisotopicCandidates.map{ peakel => peakel-> Tuple3[Float, Float, Int](peakel.computeCorrelationWith(ft.peakels(0)) toFloat,
                                                                               peakel.intensity / ft.peakels(0).intensity,
                                                                               math.abs(peakel.apexIndex - ft.peakels(0).apexIndex))
        
      }
      
      val choosenOnes = scoring.filter{ case (peakel, scores) => scores._1 > this.params.minPeakelCorrToMono &&
                                                                scores._2 > this.params.minIntensityToMono &&
                                                                scores._3 < 10 
      }.keys toArray
    }
                            
            
      
                          
                          
    null
  }
  
  
  /************************************************************************************************
   * EXPOSED FUNCTIONS 
   ************************************************************************************************/
  /**
   * fill feature overlapping related attributes 
   */
  def extractOverlappingFeatures(ft: Feature, theoIP: TheoreticalIsotopePattern, pklTree: PeakListTree) : OverlapStatus = {
      if (ft.peakels.isEmpty)
        throw new Exception("can not extract overlapping features of an empty feature. Returning Error")
      
      ft.getIsotopicPatterns.par.map( ip => this._extractOverlappingIps(ip, theoIP, pklTree) )
      val ovlFeatures = this._buildOverlappingFeatures( ft )
      this._evaluateOverlappingStatus( ft, ovlFeatures )
  }
  
  /**
   * 
   */
  def extractOverlappingFeaturesUsingWaveletApproaches( ft: Feature, theoIP: TheoreticalIsotopePattern, pklTree: PeakListTree) : OverlapStatus = {
      if (ft.peakels.isEmpty)
        throw new Exception("can not extract overlapping features of an empty feature. Returning Error")
      
      val ovlFeatures = this._extractOverlappingFeatures(ft.peakels(0), ft.charge, pklTree)
      this._evaluateOverlappingStatus(ft, ovlFeatures)
  }

}//end stuff