package fr.profi.mzdb.algo.feature.extraction

import collection.mutable.ListBuffer
import com.weiglewilczek.slf4s.Logging
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import scala.collection.mutable.ArrayBuffer

class Ms2DrivenFtExtractor(
  //val mzDbReader: MzDbReader,
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val mzTolPPM: Float,
  val maxNbPeaksInIP: Int,
  val minNbOverlappingIPs: Int,
  val maxConsecutiveGaps: Int = 2,
  val maxTimeWindow: Float = 600f,
  val minPercentageOfMaxInt: Float = 0.005f
) extends ISupervisedFtExtractor with Logging {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
    this.extractFeature(putativeFt, pklTree, putativeFt.scanId )
  }
  
  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree, startingScanId: Int ): Option[Feature] = {
    
    // Retrieve the scan header corresponding to the starting scan id
    val scanHeaderOpt = this.scanHeaderById.get(startingScanId)
    if( scanHeaderOpt == None ) return Option.empty[Feature]    
    val scanHeader = scanHeaderOpt.get
    
    // Extract isotopic patterns around the starting scan
    val ips = this.extractIsotopicPatterns(putativeFt,pklTree,scanHeader)
    val nbIps =  ips.length
    if( nbIps == 0 ) return Option.empty[Feature]
    
    // Normalize the intensity of extracted IPs
    this.normalizeIPs( ips )
    
    // Build a TMP feature to refine the extraction
    var tmpFt = new Feature( putativeFt.id, putativeFt.mz, putativeFt.charge, ips )    
    val tmpSummedXIC = tmpFt.getSummedXIC()
    val ftTime = tmpFt.elutionTime
    
    // Find all peakels in the extracted range of IPs
    val peaksIndexes = BasicPeakelFinder.findPeakelsIndexes( tmpSummedXIC._2.map(_.toDouble), 2 )
    val peaksCount = peaksIndexes.length
    val matchingPeakIdx = peaksIndexes.find( idx => ftTime >= tmpSummedXIC._1(idx._1) && ftTime <= tmpSummedXIC._1(idx._2) )
    
    // If a peakel with an index compatible with the MS2 event has been detected
    if( matchingPeakIdx != None ) {
      
      // Extract adjusted peakel isotopic patterns
      val matchingIPs = new ArrayBuffer[IsotopicPattern]()      
      for( idx <- matchingPeakIdx.get._1 to matchingPeakIdx.get._2 ) {
        matchingIPs += tmpFt.getIsotopicPattern( idx )
      }
      
      // Build adjusted feature
      tmpFt = new Feature( putativeFt.id, putativeFt.mz, putativeFt.charge, matchingIPs )  
    }
    
    // Compute overlapping features
    this.updateFtOverlappingFeatures( tmpFt, ips, this.minNbOverlappingIPs )
    
    Some(tmpFt)
  }
  
  protected def extractIsotopicPatterns( putativeFt: PutativeFeature,
                                         pklTree: PeakListTree,
                                         startingScanHeader: ScanHeader ): Array[IsotopicPattern] = {
    
    val ips = new ListBuffer[IsotopicPattern]()
    val cycleNum = startingScanHeader.getCycle
    var apexTime = startingScanHeader.getTime
    
    // Determine intensity ascendant direction
    val range = Pair(1,5)
    var ascDirection = this._getIntensityAscendantDirection( putativeFt, pklTree, cycleNum,
                                                             range, this.mzTolPPM, 1, this.maxNbPeaksInIP )

    if( ascDirection == 0 ) return ips.toArray
    
    // Extract isotopic patterns
    var curMaxIntensity = 0.0
    
    // Iterate until left and right directions have been analyzed
    var numOfAnalyzedDirections = 0
    while( numOfAnalyzedDirections < 2 ) {
      
      var timeOverRange = false
      var consecutiveGapCount = 0
      var cycleShift = 0
      //int apexScanNum = cycleNum;
      
      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      while( consecutiveGapCount <= this.maxConsecutiveGaps && !timeOverRange ) {
        
        // Decrease cycle shift if right direction
        if( ascDirection == -1 ) { cycleShift -= 1 }
        
        // Determine current cycle number
        val curCycleNum = cycleNum + cycleShift;        
        
        // Try to retrieve the scan id
        var curScanHOpt = Option.empty[ScanHeader]
        if( this.ms1ScanIdByCycleNum.contains(curCycleNum) ) {
          val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
          
          // Retrieve the wanted scan header          
          curScanHOpt = this.scanHeaderById.get(curScanId)
        }
        
        if( curScanHOpt == None ) timeOverRange = true
        else {
          val curScanH = curScanHOpt.get            
          val curTime = curScanH.getTime
            
          // TODO: check if total time does not exceed the provided threshold
          if( this.maxTimeWindow > 0 && math.abs(curTime-apexTime) > this.maxTimeWindow/2 ) timeOverRange = true
              
          val ipOpt = pklTree.extractIsotopicPattern( curScanH, putativeFt.mz, this.mzTolPPM, putativeFt.charge, this.maxNbPeaksInIP )
          if( cycleShift == 0 && ipOpt == None ) {
            // Sometimes the precursor m/z is wrong => just skip these weird cases            
            this.logger.trace( "supervised ft extraction failed at scan id=%06d & mz=%f".format(curScanH.getId, putativeFt.getMz) )
          }
          
          // Check if an isotopic pattern has been found
          if( ipOpt != None ) {
            
            val ip = ipOpt.get
            val intensity = ip.intensity
            
            // If we have peaks
            if( ip.peaks.length > 0 ) {
              
              // Set IP elution time
              //ip.getElutionTime = curTime;
              
              // search for putative overlapping peaks
              val olpIPs = this._extractOverlappingIPs( ip, pklTree )
              
              // Set overlapping IPs if at least one has been found
              val nbOlpIPs = olpIPs.length
              if( nbOlpIPs > 0 ) {
                ip.overlappingIps = olpIPs.toArray
              }

              // Add the isotopic pattern to the list of extracted IPs            
              if( ascDirection == 1 ) ips += ip
              else if( ascDirection == -1 ) ips. +=: ( ip )
              
              // Analysis of the isotopic profile intensity
              
              if( intensity > curMaxIntensity ) {
                // Update information about the apex
                curMaxIntensity = intensity
                apexTime = curTime
              }
            }
            
            // TODO : test intensity < intensityThreshold
            if( intensity == 0 || intensity < curMaxIntensity * minPercentageOfMaxInt ) consecutiveGapCount += 1
            else consecutiveGapCount = 0
          }
          else consecutiveGapCount += 1
      
          // Increase cycle shift if right direction
          if( ascDirection == 1 ) cycleShift += 1
          
        }
      }
      
      ascDirection *= -1;
      numOfAnalyzedDirections += 1
    }
    
    ips.toArray
  }
  
  protected def refinePrecursorMz( mz: Double, pklTree: PeakListTree, scanId: Int ): Option[Double] = {

    val nearestPeak = pklTree.getNearestPeak(scanId, mz,this.mzTolPPM)
    
    var newMz = Option.empty[Double]
    if( nearestPeak != None ) {
      newMz = Some(nearestPeak.get.getMz)
    }
    else {
    }
    
    newMz
  }
  
}