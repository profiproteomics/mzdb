import java.io.File
import java.io.PrintWriter

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.MzDbFeatureExtractor
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.algo.feature.scoring.FeatureEvaluator
import fr.profi.mzdb.io.reader.RunSliceDataProvider
import fr.profi.mzdb.model.PutativeFeature

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
		val featureQualities = features.map(FeatureEvaluator.computeQualityVector(_)) toArray
		val f = new PrintWriter(exportPath)
		featureQualities.zip(features).foreach(fq => f.write("" + fq._2.toString() + "\t" + fq._1.ms1Count + "\t" 
		    + fq._1.isotopesCount + "\t" + fq._1.isotopesPattern + "\t" + fq._1.signalFluctuation + "\t" + 
		    fq._1.peakelsWidth + "\t" +fq._1.peakelsCorrelation + "\t" +fq._1.overlappingFactor + "\t" + 
		    fq._1.overlappingPeakelsCorrelation + "\n"))
		f.close()
		/*val numBins = math.sqrt(featureQualities.length) toInt
		val f2 = new PrintWriter(exportPath2)
		//ms1count
		val ms1Counts = featureQualities.par.map(fq => fq.qualityCriteria.ms1Count toDouble) toArray
		val (ms1CountsBins, binSize) = calcHistogram(ms1Counts, ms1Counts.min, ms1Counts.max, numBins)
		f2.write("ms1Counts binSize:" + binSize + "\n")
		ms1CountsBins.foreach(x => f2.write("" + x + "\n"))
		
		val isotopesCount = featureQualities.map(fq => fq.qualityCriteria.isotopesCount toDouble)
    val (isotopesCountBins, isotopesCountbinSize) = calcHistogram(isotopesCount, isotopesCount.min, isotopesCount.max, numBins)
    f2.write("isotopesCount binSize:" + isotopesCountbinSize + "\n")
    isotopesCountBins.foreach(x => f2.write("" + x + "\n"))
    
    val isotopesPattern = featureQualities.map(fq => fq.qualityCriteria.isotopesPattern toDouble)
    val (isotopesPatternBins, isotopesPatternbinSize) = calcHistogram(isotopesPattern, isotopesPattern.min, isotopesPattern.max, numBins)
    f2.write("isotopesPattern binSize:" + isotopesPatternbinSize + "\n")
    isotopesPatternBins.foreach(x => f2.write("" + x + "\n"))
    
    val signalFluctuation = featureQualities.map(fq => fq.qualityCriteria.signalFluctuation toDouble)
    val (signalFluctuationBins, signalFluctuationbinSize) = calcHistogram(signalFluctuation, signalFluctuation.min, signalFluctuation.max, numBins)
    f2.write("signalFluctuation binSize:" + signalFluctuationbinSize + "\n")
    signalFluctuationBins.foreach(x => f2.write("" + x + "\n"))
    
    val peakelsWidth = featureQualities.map(fq => fq.qualityCriteria.peakelsWidth toDouble)
    val (peakelsWidthBins, peakelsWidthbinSize) = calcHistogram(peakelsWidth, peakelsWidth.min, peakelsWidth.max, numBins)
    f2.write("peakelsWidth binSize:" + peakelsWidthbinSize + "\n")
    peakelsWidthBins.foreach(x => f2.write("" + x + "\n"))
    
    val peakelsCorrelation = featureQualities.filter(_.qualityCriteria.peakelsCorrelation.toString() != "NaN").map(fq => if (fq.qualityCriteria.peakelsCorrelation < 0) 0 else fq.qualityCriteria.peakelsCorrelation toDouble)
    peakelsCorrelation.foreach(println(_))
    val (peakelsCorrelationBins, peakelsCorrelationbinSize) = calcHistogram(peakelsCorrelation, peakelsCorrelation.min, peakelsCorrelation.max, numBins)
    println("" + peakelsCorrelation.min + ", " + peakelsCorrelation.max)
    f2.write("peakelsCorrelation binSize:" + peakelsCorrelationbinSize + "\n")
    peakelsCorrelationBins.foreach(x => f2.write("" + x + "\n"))
		
    val overlappingFactor = featureQualities.map(fq => fq.overlapCriteria.overlappingFactor toDouble)
    val (overlappingFactorBins, overlappingFactorbinSize) = calcHistogram(overlappingFactor, overlappingFactor.min, overlappingFactor.max, numBins)
    f2.write("overlappingFactor binSize:" + overlappingFactorbinSize + "\n")
    overlappingFactorBins.foreach(x => f2.write("" + x + "\n"))

    f2.close()*/
		
}