package fr.profi.mzdb.algo.feature.extraction

import collection.mutable.HashMap
import util.control.Breaks._
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.IsotopicPattern
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.signal.detection.CwtPeakel
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.Ridge
import fr.profi.mzdb.utils.math.wavelet.RidgesFinder



class PredictedTimeFtExtractor(
  override val mzDbReader: MzDbReader,
  override val scanHeaderById: Map[Int, ScanHeader],
  override val nfByScanId: Map[Int, Float],
  override val mzTolPPM: Float,
  override val maxNbPeaksInIP: Int,
  override val minNbOverlappingIPs: Int,
  val minConsecutiveScans: Int = 4,
  val predictedTimeTol: Int = 120) extends Ms2DrivenFtExtractor (
  mzDbReader,
  scanHeaderById,
  nfByScanId,
  mzTolPPM,
  maxNbPeaksInIP,
  minNbOverlappingIPs)  with RidgesFinder {

  override def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    //System.out.println("extracting feature with m/z="+putativeFt.mz +" at time="+putativeFt.elutionTime);
    val startingScanId = this._findStartingScanId(putativeFt, pklTree)
    //val startingScanIdCwt = _findStartingScanIDUsingCwt(putativeFt, pklTree)

    if (startingScanId > 0)
      return super.extractFeature(putativeFt, pklTree, startingScanId)
    else
      return Option.empty[Feature]

  }

  /**
   * use wavelet technique to dertermine starting point to extract
   */
  def extractFeatureCwt(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    val startingScanIdCwt = _findStartingScanIDUsingCwt(putativeFt, pklTree)
    if (startingScanIdCwt > 0)
      return super.extractFeature(putativeFt, pklTree, startingScanIdCwt)
    else
      return Option.empty[Feature]
  }

  private def _findStartingScanIDUsingCwt(putativeFt: PutativeFeature, pklTree: PeakListTree): Int = {
    //extract some vars
    val elutionTime = putativeFt.elutionTime
    val mz = putativeFt.mz
    val charge = putativeFt.charge

    val curScanH = mzDbReader.getScanHeaderForTime(elutionTime, 1)
    val leftmostScanH = mzDbReader.getScanHeaderForTime(elutionTime - predictedTimeTol, 1)
    val rightmostScanH = mzDbReader.getScanHeaderForTime(elutionTime + predictedTimeTol, 1)

    val scanIDs = pklTree.scansIDs().filter(x => x > (curScanH.getId - leftmostScanH.getId) && x < (curScanH.getId + rightmostScanH.getId)) toArray

    val NB_PEAKELS_TO_CHECK = 3
    val deltaMass = 1.002f / charge

    var peakels = new ArrayBuffer[Array[CwtPeakel]]
    var values = new ArrayBuffer[Array[Float]]
    
    breakable {
	    for (c <- 0 to NB_PEAKELS_TO_CHECK) {
	      
	      var mzToCheck = (deltaMass * c) + mz
	      //extractPeaks
	      var peaks = _extractPeaks(putativeFt, pklTree, scanIDs, mzToCheck, mzTolPPM)
	      values += peaks.map(_.getIntensity)
	      //build cwt
	      val peakelFinder = new WaveletBasedPeakelFinder( peaks, scales = (1f to 64f by 1f).toArray, wavelet = MexicanHat() ) //mexh by default
	      var peakelsInPredictedRange = peakelFinder.findCwtPeakels().filter( x => x.apexLcContext.getScanId() > leftmostScanH.getId && x.apexLcContext.getScanId() < rightmostScanH.getId )
	      
	      //we break if did not find any peakel ?
	      if (peakelsInPredictedRange.isEmpty)
	        break
	      
	      peakels += peakelsInPredictedRange
	      
	      //val closestPeakel = peakelsInPredictedRange.sortBy(x => math.abs(curScanH.getId - x.scanID)).first
	    }
    }
    
    var ridges = ridgeCalc(peakels)
    if (ridges.isEmpty) {
      logger.warn("no signal found in selected region of the calculated XIC")
      return 0
    }
    var weightedRidges = _rmsdCalc(peakels, ridges, values)
    
    //we take the minimum
    var bestCandidateRidge = weightedRidges.map{ case (ridge, rmsds) => (ridge, rmsds.sum[Double] / rmsds.length) }.toList.sortBy(x => x._2).first._1
    //return the maxIdx (scanId) of the ridge that has the most intense value at monoistopic peakel 
    bestCandidateRidge.lastScaleMaxCoeffPos._1
  }

  private def _extractPeaks(putativeFt: PutativeFeature, pklTree: PeakListTree, selectedScanIDs: Array[Int], mz: Double, mzTol: Float): Array[Peak] = {
    var peaks = new ArrayBuffer[Peak]
    selectedScanIDs.foreach { x =>
      var minmz = putativeFt.mz - (mzTolPPM * putativeFt.mz / 1e6)
      var maxmz = putativeFt.mz + (mzTolPPM * putativeFt.mz / 1e6)
      peaks += pklTree.getPeaksInRange(x, minmz, maxmz).sortBy(_.getIntensity).last
    }
    peaks.toArray
  }

  /**
   * return the most probable Ridges
   */
  private def ridgeCalc(peakels: ArrayBuffer[Array[CwtPeakel]] ) : Array[Ridge] = {
    //var apexes = new ArrayBuffer[ArrayBuffer[Int]]
    var apexes = peakels.map {x=> x.map {_.apex} } toArray
    var ridges = this.findRidges(apexes.reverse, null, winLength = 10) //10scans aprroximatively 20-30 s
    
    var ridgesByLength = new HashMap[Int, ArrayBuffer[Ridge]]
    ridges.foreach( x => ridgesByLength.getOrElseUpdate(x.length, new ArrayBuffer[Ridge]) += x)
    var longestRidges = ridgesByLength(ridgesByLength.keys.toList.sortBy(x=>x).last)
    longestRidges.toArray
  }
  
  /**
   * lastScale correspond to the monoisotopic peakel
   * return an hashmap containing Ridge and an array of rmsd using monoistopic peakel
   */
  private def _rmsdCalc(peakels: ArrayBuffer[Array[CwtPeakel]], 
		  		        ridges: Array[Ridge], 
		  		        values: ArrayBuffer[Array[Float]]) : HashMap[Ridge, Array[Double]] = {
    
    var rpeakels = peakels.reverse
    var rvalues = values.reverse
    
    var weightedRidges = new HashMap[Ridge, Array[Double]]
    
    for (ridge <- ridges) {
      //var peaks = new HashMap[Int, Array[Float]]
      
      var correspondingPeakels = new HashMap[Int, CwtPeakel]
      ridge.maximaIndexPerScale.foreach{ case (a, b)  =>  if (b!= None) peakels(a).foreach {x => if (x.apex == b.get) correspondingPeakels(a) = x}  }    
      var (monoisotopicScale, monoisotopicPeakel) = correspondingPeakels.map { case (a, b) => (a, b) }.toList.sortBy(x => x._1).last//sList.sortBy( peakel => math.abs(peakel.maxIdx - peakel.minIdx)).last
      //val longestSize = math.abs(longestPeakel.maxIdx - longestPeakel.minIdx) //abs not necessary i suppose
      var (minIdx, maxIdx) = (monoisotopicPeakel.minIdx, monoisotopicPeakel.maxIdx)
      
      //peaks += scale -> values(scale).slice(minIdx, maxIdx)
      
      var rmsds = new ArrayBuffer[Double]
      for ( (scale, peakel) <- correspondingPeakels if peakel != monoisotopicPeakel) {
        //var (minIdx_ , maxIdx_) = (peakel.minIdx, peakel.maxIdx)
        var array = zeroPad(peakel, values(scale), minIdx, maxIdx)
        // calc rmsd
        rmsds += VectorSimilarity.rmsd(array.map(_.toDouble), values(monoisotopicScale).map(_.toDouble))
      }
      weightedRidges += ridge -> rmsds.toArray
    }
    weightedRidges
  }
  
  
  /**
   * use to make peakels contain same number of peaks, if shorter use zero padding, 
   * else remove value to match min and max Idx of the monoisotopic peakel
   */
  private def zeroPad(peakel: CwtPeakel, values: Array[Float], minIdx: Int, maxIdx:Int) : Array[Float]= {
    var (minIdx_ , maxIdx_) = (peakel.minIdx, peakel.maxIdx)
    var output = values.slice(minIdx_, maxIdx_).toBuffer
    
    while (minIdx_ < minIdx) {
      output.remove(0)
      minIdx_ += 1
    }
    
    while (minIdx_ > minIdx) {
      output.insert(0, 0f)
      minIdx_ -= 1
    }
    
    while (maxIdx_ < maxIdx) {
      output += 0f
      maxIdx_ +=1
    }
    
    while (maxIdx_ > maxIdx) {
      output.remove(output.length - 1)
      maxIdx_ -= 1
    }
    
    output.toArray
  } 
  

  private def _findStartingScanId(putativeFt: PutativeFeature, pklTree: PeakListTree): Int = {

    // Retrieve some vars
    val elutionTime = putativeFt.elutionTime
    val curScanH = this.mzDbReader.getScanHeaderForTime(elutionTime, 1)
    var curCycleNum = curScanH.getCycle

    val leftmostScanH = this.mzDbReader.getScanHeaderForTime(elutionTime - predictedTimeTol, 1)
    val rightmostScanH = this.mzDbReader.getScanHeaderForTime(elutionTime + predictedTimeTol, 1)
    val leftmostCycleNum = leftmostScanH.getCycle
    val rightmostCycleNum = rightmostScanH.getCycle

    // Compute the cycle range which will be screened to search for signal
    var cycleShift = 0
    if (Math.abs(rightmostCycleNum - curCycleNum) >= Math.abs(leftmostCycleNum - curCycleNum)) {
      cycleShift = Math.abs(rightmostCycleNum - curCycleNum);
    } else {
      cycleShift = Math.abs(leftmostCycleNum - curCycleNum);
    }
    val range = Pair(0, cycleShift)

    // Search for signal while progressively decreasing the number of required peaks in extracted IPs
    var startingScanId = 0

    breakable {
      for (minNbPeaks <- this.maxNbPeaksInIP to 1 by -1) {

        // Search for the direction which exhibits the highest intensity
        val intensityAscDir = _getIntensityAscendantDirection(putativeFt, pklTree, curCycleNum, range,
          this.mzTolPPM, minNbPeaks, minNbPeaks)

        // Check if there is some signal in one of the left/right directions
        if (intensityAscDir != 0) {

          var timeShift = 0f
          var nbConsecutiveScans = 0
          var nbConsecutiveGaps = 0
          var breaked = false

          while (timeShift < predictedTimeTol && !breaked) {

            // Try to retrieve the scan id
            if (!this.ms1ScanIdByCycleNum.contains(curCycleNum)) breaked = true
            else {
              val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
              val startingScanH = this.scanHeaderById(curScanId)

              // Try to extract some signal
              val ipOpt = pklTree.extractIsotopicPattern(startingScanH, putativeFt.mz, this.mzTolPPM,
                putativeFt.charge, this.maxNbPeaksInIP);

              // Check if signal has been extracted
              if (ipOpt != None && ipOpt.get.peaks.length >= minNbPeaks) {
                nbConsecutiveScans += 1
                nbConsecutiveGaps = 0
                startingScanId = startingScanH.getId
              } else {
                startingScanId = 0
                nbConsecutiveGaps += 1

                if (nbConsecutiveGaps > this.maxConsecutiveGaps) nbConsecutiveScans = 0

              }

              // Stop the signal extractions if we have enough consecutive scans
              if (nbConsecutiveScans >= minConsecutiveScans) breaked = true
              else {
                curCycleNum += intensityAscDir
                timeShift = Math.abs(startingScanH.getTime - elutionTime)
              }
            }
          }
        }

        if (startingScanId != 0) break

      }
    }

    startingScanId

    /* {
    my( $self, $charge, $moz, $mozTol, $rawFileParser, $scanNumber, $scanTime, $quantitationMethod ) = @_;
    my $predictedTimeTol = $quantitationMethod->predictedTimeTol;
    my $stepSize = $quantitationMethod->stepSize;
    my $intensityThreshold = $quantitationMethod->intensityThreshold;

    my( @startingPoint, @deltaMoz, @deltaTime );

    ### Forward search
    $deltaTime[0] = 0;
    my $curScanNumber = $scanNumber + 1;
    while( $deltaTime[0] < $predictedTimeTol )
      {
      my $scan = $rawFileParser->getScan('number',$curScanNumber); last if not defined $scan;
      my $isotopicProfile = $scan->peaks->extractIsotopicProfile($charge, $moz, $mozTol );
      if( defined $isotopicProfile and $isotopicProfile->getTrueNumOfPeaks >= 2 )
        {
        ### Extract the current isotopic profile intensity
        my $intensity = $isotopicProfile->computeIntensity;
        if( $intensity > $intensityThreshold  )
          {
          $startingPoint[0] = $curScanNumber;
          $deltaMoz[0] = abs($isotopicProfile->getDeltaMoz( $moz ));
          last;
          }
        }

      $deltaTime[0] = abs( $scan->retentionTime - $scanTime );
      $curScanNumber += $stepSize;
      }

    ### Backward search
    $deltaTime[1] = 0;
    my $curScanNumber = $scanNumber - 1;
    while( $deltaTime[1] < $predictedTimeTol )
      {
      my $scan = $rawFileParser->getScan('number',$curScanNumber); last if not defined $scan;
      my $isotopicProfile = $scan->peaks->extractIsotopicProfile($charge, $moz, $mozTol );
      if( defined $isotopicProfile and $isotopicProfile->getTrueNumOfPeaks >= 2 )
        {
        ### Extract the current isotopic profile intensity
        my $intensity = $isotopicProfile->computeIntensity;
        if( $intensity > $intensityThreshold )
          {
          $startingPoint[1] = $curScanNumber;
          $deltaMoz[1] = abs($isotopicProfile->getDeltaMoz( $moz ));
          last;
          }
        }
        
      $deltaTime[1] = abs( $scan->retentionTime - $scanTime );
      $curScanNumber -= $stepSize;
      }

    if( !defined $deltaMoz[0] and !defined $deltaMoz[1] ) { return undef; }
    elsif( defined $deltaMoz[0] and !defined $deltaMoz[1] ) { return $startingPoint[0]; }
    elsif( defined $deltaMoz[1] and !defined $deltaMoz[0] ) { return $startingPoint[1]; }
    elsif( $deltaTime[0] ne $deltaTime[1] )
      { return $deltaTime[0] < $deltaTime[1] ? $startingPoint[0] : $startingPoint[1]; }
    else { return $deltaMoz[0] < $deltaMoz[1] ? $startingPoint[0] : $startingPoint[1]; }

    #elsif( $delta[0] eq $delta[1] ) #Keep the best intensity if there are two different points iwth the same m/z
    #  {
    #  my @intensitySums;
    #  $intensitySums[0] = $self->_integrateIntensity($charge, $moz, $mozTol, $rawFileParser, $startingPoint[0], [1,5], 1);
    #  $intensitySums[1] = $self->_integrateIntensity($charge, $moz, $mozTol, $rawFileParser, $startingPoint[1], [1,5], -1);
    #  return $intensitySums[0] > $intensitySums[1] ? $startingPoint[0] : $startingPoint[1];
    #  }

    return undef;
    }*/
  }

}
