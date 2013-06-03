package fr.profi.mzdb.algo.feature.scoring

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.model.Feature
import scala.reflect.BeanProperty
import util.control.Breaks._
import fr.profi.mzdb.utils.misc.IsotopicPatternLookup
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.signal.fitting.GaussFitter
import org.apache.commons.math.stat.descriptive.moment.StandardDeviation
import fr.profi.mzdb.algo.signal.fitting.PolyFitter
import fr.profi.mzdb.algo.signal.fitting.GaussLorentzFitter
import org.apache.commons.math.optimization.OptimizationException

object FeatureScorer {
  
  // TODO: ms1_count
  // Compute the distribution of log(ms1_count)
  // Then compute the bounds using the box plot method
  // First results min = 5 ; max = 110
	
  /*****************************************************************
   *  Estimation of the correlation factor
   ****************************************************************/
  
  /**mean correlation */
  def calcMeanPeakelCorrelation( peakels: Seq[Peakel] ): Double = {
    
    val peakelsCount = peakels.length 
    if( peakelsCount < 2 ) return 0.0
    
    var peakelCorrSum = 0.0
    peakels.sliding(2).foreach { case Seq(p1, p2) => 
      peakelCorrSum += calcPeakelCorrelation(p1, p2)
    }
    
    peakelCorrSum / (peakelsCount-1)
  }
  
  /**correlation between to peakels*/
  def calcPeakelCorrelation( firstPeakel: Peakel, secondPeakel: Peakel ): Double = {
    val secondPeakelPeakByTime = secondPeakel.getPeakByElutionTime
    
    val firstPeakelIntensities = new ArrayBuffer[Float]()
    val secondPeakelIntensities = new ArrayBuffer[Float]()
    
    // Compute the peak intersection of the two peakels
    var peakIndex = 0
    for( peakelPeak <- firstPeakel.definedPeaks ) {
      
      val secondPeakelPeak = secondPeakelPeakByTime.get( peakelPeak.getLcContext.getElutionTime )
      
      if( secondPeakelPeak != None ) {
        firstPeakelIntensities += peakelPeak.getIntensity() 
        secondPeakelIntensities += secondPeakelPeak.get.getIntensity()
      }
      peakIndex += 1
    }
    return VectorSimilarity.pearsonCorrelation( firstPeakelIntensities.toArray, secondPeakelIntensities.toArray );
  }
  
  
  /*****************************************************************
   *  Estimation of the overlapping factor
   *  For the moment, an overlapping factor is computed based 
   *  only the first peakel of the considered feature 
   ****************************************************************/
  
  def calcOverlappingFactor(f: Feature,  mzTolInPpm: Double) : Double = {
    if (f.overlappingFeatures == null) 
      return 0d
      
    var of = 0d
    val mz = f.getMz
    val mzTolInDa =  mz * mzTolInPpm / 1e6;
    val overlappingMap = new ArrayBuffer[ Tuple3[Peakel, Option[Peakel], Option[Peakel]] ]()
    val peakel = f.peakels(0) //monoistopic peakel
    //for (peakel <- f.getPeakels) {
    val refScanID = peakel.getApexScanContext.getScanId()
    //get the best overlapping peakel in overlapping feature set
    var (leftOverlappingPeaks, rightOverlappingPeaks) = (Option.empty[Peakel], Option.empty[Peakel])
    
    f.overlappingFeatures.foreach{ ofeature => ofeature.getPeakels.foreach{ p => 
                                  if ( (p.getMz - mz) < mzTolInDa) {
                                    if (p.getApexScanContext.getScanId() < refScanID) {
                                      leftOverlappingPeaks = Some(p)
                                    }
                                    if (p.getApexScanContext.getScanId() > refScanID) {
                                      rightOverlappingPeaks = Some(p)
                                    }
                                  }
                             }
    overlappingMap +=  Tuple3(peakel, leftOverlappingPeaks, rightOverlappingPeaks)
    }
    calcMeanOverlappingFactor(overlappingMap)
  }
  
  
  def calcMeanOverlappingFactor( peakels: Seq[Tuple3[Peakel, Option[Peakel], Option[Peakel]]] ) : Double = {
    if (peakels.isEmpty)
      return 0d
    var m = 0d
    peakels.foreach(p => m += calcOverlappingFactor(p._1, p._2, p._3))
    m / peakels.length
  }
  
  
  /**
   * overlap factor weighted with the intensity of the overlapping peakel
   * (the overlapping factor for one peakel should be bigger when the overlapping
   * peakel is a lot more intense.)
   *  
   */
  def calcOverlappingFactor ( peakel : Peakel, 
                              leftOverlappingPeakel: Option[Peakel], 
                              rightOverlappingPeakel: Option[Peakel], baseline : Float = 0): Double = {
    
    /** almost the 2 same function ugly */
    var of = 0d // in case of no overlap
    
    val calcLeftOverlappingFactor = (p : Peakel, lop : Peakel) => {
      val (overlappingIntens, refIntens) = ( p.definedPeaks.head.getIntensity, p.definedPeaks.last.getIntensity)
       of += (overlappingIntens / refIntens) * (lop.getApex.getIntensity() / p.getApex.getIntensity())
       //of += 
    }
    
    val calcRightOverlappingFactor = (p : Peakel, rop : Peakel) => {
     val (overlappingIntens, refIntens) = ( p.definedPeaks.last.getIntensity, p.definedPeaks.head.getIntensity )
       of += (overlappingIntens / refIntens) * (rop.getApex.getIntensity() / p.getApex.getIntensity())
       //of += rop.getApex.getIntensity() / p.getApex.getIntensity()
    }
    
    val (mz, intens) = (peakel.getApex.getMz(), peakel.getApex.getIntensity())
    //the intersection point must at the end of the first peakel and at the beginning of the second peakel
    //this point is supposed to be shared by the two overlapping peakels.
    val lastIntens = peakel.definedPeaks.last.getIntensity()
    val firstIntens = peakel.definedPeaks.head.getIntensity()
    
    if (leftOverlappingPeakel == None && rightOverlappingPeakel == None) {
      //do nothing
    }else if (leftOverlappingPeakel != None && rightOverlappingPeakel == None) {
      calcLeftOverlappingFactor(peakel, leftOverlappingPeakel.get)
    } else if (leftOverlappingPeakel == None && rightOverlappingPeakel != None){
      calcRightOverlappingFactor(peakel, rightOverlappingPeakel.get)
      //several overlapping peaks detected
    } else {
      calcLeftOverlappingFactor(peakel, leftOverlappingPeakel.get)
      calcRightOverlappingFactor(peakel, rightOverlappingPeakel.get)
    }
    of
  }
  
  /*****************************************************************
   *  Estimation of the quality of the Isotopic Pattern
   *  rmsd of peakel's area observed vs peakel's area calculated
   ****************************************************************/
  def calcIsotopicDistance(f : Feature): Double = {
    //if (f.peakelsCount < 2)
    val mz = f.getMz
    
    // Retrieve theoreticl and observed abundances
    val theoPattern = IsotopicPatternLookup.getTheoreticalPattern(mz,f.getCharge)
    val theoAbundances = theoPattern.relativeAbundances.map(_.toDouble)
    val obsAbundances = f.getPeakels.map(_.area.toDouble)
    
    // Normalize observed abundances
    val maxIntens = obsAbundances.max
    val normAbundances = obsAbundances.map(_ * 100 / maxIntens)
    
    val (shortest, longest) = if (theoAbundances.length < normAbundances.length) (theoAbundances, obsAbundances)
    else (obsAbundances, theoAbundances)
    
    VectorSimilarity.rmsd( shortest, longest.take(shortest.length) )
  }
  
  /*****************************************************************
   *  Perform a local max detection on each peakels
   ****************************************************************/
  
  /** using the basic peakel finder.*/
  def calcSignalFluctuationByBasicPeakelFinder ( f: Feature): Float =  {
  // a perfect shape would be only one maximum index
    var shape = 0f
    f.getPeakels.foreach(p=> shape += BasicPeakelFinder.findPeakelsIndexes(p.definedPeaks).length)
    shape / f.peakelsCount
  }
  
  /**using the wavelet peakel finder*/
  def calcSignalFluctuationByWaveletBasedPeakelFinder ( f:Feature ): Float =  {
    var shape = 0f
    f.getPeakels.foreach(p=> shape +=  new WaveletBasedPeakelFinder(p.definedPeaks).findCwtPeakels().length)
    shape / f.peakelsCount
  }
  
  /*****************************************************************
   *  Estimation of the shape
   *  weighting of the rmsd value (fit vs observed) by the peakel area
   *  return the weighted mean 
   ****************************************************************/
  
  /**using gaussLorentz fitting*/
  def calcShapeByGaussLorentzFitting (f: Feature) : Float =  {
     val rmsds = f.peakels.map{ p =>
         val xObs = p.definedPeaks.map(_.getMz)
         val yObs = p.definedPeaks.map(_.getIntensity.toDouble)
         val gaussFitter = new GaussLorentzFitter(p.definedPeaks.map(_.getMz), yObs)
         gaussFitter.optimize()
         val refPeak = gaussFitter.peaks(0)
         val yModelized = refPeak.getFittedY(xObs)
         VectorSimilarity.rmsd(yObs, yModelized) * p.getArea
         }
       rmsds.sum / f.getPeakels.map(_.getArea).sum toFloat
  }
  
  /**using gaussian fitting*/
   def calcShapeByGaussFitting (f: Feature) : Float =  {
     val rmsds = f.peakels.map{ p =>
       val xObs = p.definedPeaks.map(_.getMz)
       val yObs = p.definedPeaks.map(_.getIntensity.toDouble)
       if (yObs.length >= 3) {//minimum points to perform a Non Linear Square Fitting (Apache Commons Math)
         val gaussFitter = new GaussFitter(p.definedPeaks.map(_.getMz), yObs)
         try {
           gaussFitter.optimize()
             val refPeak = gaussFitter.peaks(0)
             val yModelized = refPeak.getFittedY(xObs)
             VectorSimilarity.rmsd(yObs, yModelized) * p.getArea
         }catch {
           case optimzExcept : OptimizationException => 1000
         }
       
       }else {
         0
       }
       }
     rmsds.sum / f.getPeakels.map(_.getArea).sum toFloat  
   }
    
   /**using parabola fitting*/
   def calcShapeByParabolaFitting (f: Feature) : Float =  {
    val rmsds = f.peakels.map{ p =>
       val xObs = p.definedPeaks.map(_.getMz)
       val yObs = p.definedPeaks.map(_.getIntensity.toDouble)
       val gaussFitter = new PolyFitter(p.definedPeaks.map(_.getMz), yObs)
       gaussFitter.optimize()
       val refPeak = gaussFitter.peaks(0)
       val yModelized = refPeak.getFittedY(xObs)
       VectorSimilarity.rmsd(yObs, yModelized) * p.getArea
       }
     rmsds.sum / f.getPeakels.map(_.getArea).sum toFloat
  }
  
  /*****************************************************************
   *  Evaluation of the peakel width 
   ****************************************************************/
  
   /**mean of the medians of each peakel width*/
  def calcMeanOfMedianPeakelsWidth(f:Feature): Float = {
    var peakelsWidth = 0f
    for (peakel <- f.getPeakels) {
       val peaks = peakel.getDefinedPeaks.sortBy(x => x.getLeftHwhm() + x.getRightHwhm())
       val p = peaks((0.5 * peaks.length).toInt)
       peakelsWidth += p.getLeftHwhm() + p.getRightHwhm()
    }
    peakelsWidth / f.getPeakelsCount
  }
  
  /**compute median of all peakels width*/
  def calcMedianPeakelsWidth(f:Feature): Float = {
    var peakelsWidthMedian = new ArrayBuffer[Float]
    for (peakel <- f.getPeakels) {
       peakelsWidthMedian ++ peakel.getDefinedPeaks.map(x => x.getLeftHwhm() + x.getRightHwhm())
    }
    peakelsWidthMedian = peakelsWidthMedian.sorted
    peakelsWidthMedian((0.5f * peakelsWidthMedian.length) toInt)
  }
  
  /**
   * standard deviation on peak width is calculated for each peakel
   * it is weighted by the intensity of the evaluated peakel 
   */
  def calcStdDevPeakelsWidth (f : Feature) :Float = {
    var peakelsWidth = new ArrayBuffer[Double]
    for (peakel <- f.getPeakels) {
       val stdDev = new StandardDeviation().evaluate(peakel.getDefinedPeaks.map(x => (x.getLeftHwhm() + x.getRightHwhm()) toDouble))
       peakelsWidth += stdDev * peakel.getArea
    }
    if (!peakelsWidth.isEmpty)
      peakelsWidth.sum / f.getPeakels.map(_.getArea).sum toFloat
    else 0f
  }
  
}