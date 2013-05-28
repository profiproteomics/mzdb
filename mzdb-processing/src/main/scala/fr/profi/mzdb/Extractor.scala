

import fr.profi.mzdb.MzDbReader
import java.io.File
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Scan
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.MzDbFeatureExtractor
import fr.profi.mzdb.io.reader.RunSliceDataProvider
import fr.profi.mzdb.utils.misc.IsotopicPatternLookup
import fr.profi.mzdb.algo.feature.scoring.FeatureEvaluator
import fr.profi.mzdb.algo.feature.scoring.FeatureEvaluationThresholds
import java.io.PrintWriter




object Extractor extends App {
  
    def calcHistogram(data:Array[Double], min:Double, max: Double,  numBins:Int): Pair[Array[Int], Double] = {
        val result = new Array[Int](numBins);
        val binSize = (max - min)/numBins;
      
        for ( d <- data) {
          val bin = ((d - min) / binSize).toInt
          if (bin < 0) { /* this data is smaller than min */ }
          else if (bin >= numBins) { /* this data point is bigger than max */ }
          else {
            result(bin) += 1;
          }
        }
        (result, binSize)
    }
    
		// Instantiate the mzDB
		val dbPath = "D:\\LCMS\\raw_files\\OENYD100205_16_medium.raw.mzdb"
		val exportPath = "D:\\LCMS\\raw_files\\OENYD100205_16_medium_ft_quality.txt"
		val exportPath2 = "D:\\LCMS\\raw_files\\OENYD100205_16_medium_ft_histo.txt"

		var mzDbInstance:MzDbReader  = null;
		try {
			mzDbInstance = new MzDbReader(new File(dbPath), true)
		} catch  {
		  case e: Exception => e.printStackTrace()
		}
		val pf = new ArrayBuffer[PutativeFeature];
		val it = mzDbInstance.getMsScanIterator(2);
		while (it.hasNext()) {
			val scan = it.next();
			val feature = new PutativeFeature(PutativeFeature.generateNewId, scan.getHeader().getPrecursorMz(), scan.getHeader().getPrecursorCharge());
			feature.setScanId(scan.getHeader().getId())
			feature.setIsPredicted(false)
			pf += feature
		}
		val extractor = new MzDbFeatureExtractor(mzDbInstance)
		val runSliceDataProvider = new RunSliceDataProvider(mzDbInstance)
		val features = extractor.extractFeatures(runSliceDataProvider, pf, 5)
		val featuresQuality = features.map(FeatureEvaluator.evaluate(_, None)) toArray
		val f = new PrintWriter(exportPath)
		featuresQuality.foreach(fq => f.write("" + fq.feature.toString() + "\t" + fq.qualityCriteria.ms1Count + "\t" 
		    + fq.qualityCriteria.isotopesCount + "\t" + fq.qualityCriteria.isotopesPattern + "\t" + fq.qualityCriteria.signalFluctuation + "\t" + 
		    fq.qualityCriteria.peakelsWidth + "\t" +fq.qualityCriteria.peakelsCorrelation + "\t" +fq.overlapCriteria.overlappingFactor + "\t" + 
		    fq.overlapCriteria.overlappingPeakelsCorrelation + "\n"))
		f.close()
		val numBins = math.sqrt(featuresQuality.length) toInt
		val f2 = new PrintWriter(exportPath2)
		//ms1count
		val ms1Counts = featuresQuality.par.map(fq => fq.qualityCriteria.ms1Count toDouble) toArray
		val (ms1CountsBins, binSize) = calcHistogram(ms1Counts, ms1Counts.min, ms1Counts.max, numBins)
		f2.write("ms1Counts binSize:" + binSize + "\n")
		ms1CountsBins.foreach(x => f2.write("" + x + "\n"))
		
		val isotopesCount = featuresQuality.map(fq => fq.qualityCriteria.isotopesCount toDouble)
    val (isotopesCountBins, isotopesCountbinSize) = calcHistogram(isotopesCount, isotopesCount.min, isotopesCount.max, numBins)
    f2.write("isotopesCount binSize:" + isotopesCountbinSize + "\n")
    isotopesCountBins.foreach(x => f2.write("" + x + "\n"))
    
    val isotopesPattern = featuresQuality.map(fq => fq.qualityCriteria.isotopesPattern toDouble)
    val (isotopesPatternBins, isotopesPatternbinSize) = calcHistogram(isotopesPattern, isotopesPattern.min, isotopesPattern.max, numBins)
    f2.write("isotopesPattern binSize:" + isotopesPatternbinSize + "\n")
    isotopesPatternBins.foreach(x => f2.write("" + x + "\n"))
    
    val signalFluctuation = featuresQuality.map(fq => fq.qualityCriteria.signalFluctuation toDouble)
    val (signalFluctuationBins, signalFluctuationbinSize) = calcHistogram(signalFluctuation, signalFluctuation.min, signalFluctuation.max, numBins)
    f2.write("signalFluctuation binSize:" + signalFluctuationbinSize + "\n")
    signalFluctuationBins.foreach(x => f2.write("" + x + "\n"))
    
    val peakelsWidth = featuresQuality.map(fq => fq.qualityCriteria.peakelsWidth toDouble)
    val (peakelsWidthBins, peakelsWidthbinSize) = calcHistogram(peakelsWidth, peakelsWidth.min, peakelsWidth.max, numBins)
    f2.write("peakelsWidth binSize:" + peakelsWidthbinSize + "\n")
    peakelsWidthBins.foreach(x => f2.write("" + x + "\n"))
    
    val peakelsCorrelation = featuresQuality.filter(_.qualityCriteria.peakelsCorrelation.toString() != "NaN").map(fq => if (fq.qualityCriteria.peakelsCorrelation < 0) 0 else fq.qualityCriteria.peakelsCorrelation toDouble)
    peakelsCorrelation.foreach(println(_))
    val (peakelsCorrelationBins, peakelsCorrelationbinSize) = calcHistogram(peakelsCorrelation, peakelsCorrelation.min, peakelsCorrelation.max, numBins)
    println("" + peakelsCorrelation.min + ", " + peakelsCorrelation.max)
    f2.write("peakelsCorrelation binSize:" + peakelsCorrelationbinSize + "\n")
    peakelsCorrelationBins.foreach(x => f2.write("" + x + "\n"))
		
    val overlappingFactor = featuresQuality.map(fq => fq.overlapCriteria.overlappingFactor toDouble)
    val (overlappingFactorBins, overlappingFactorbinSize) = calcHistogram(overlappingFactor, overlappingFactor.min, overlappingFactor.max, numBins)
    f2.write("overlappingFactor binSize:" + overlappingFactorbinSize + "\n")
    overlappingFactorBins.foreach(x => f2.write("" + x + "\n"))

    f2.close()
		
}