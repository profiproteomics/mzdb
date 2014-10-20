package fr.profi.mzdb.io.exporter

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter
import java.util.Arrays

import scala.collection.mutable.HashMap
 
//import com.codahale.jerkson.Json.generate
import fr.profi.mzdb.model.Feature


/**
 * @author David Bouyssie
 *
 */
object MzTSVFeatureWriter {
  
  val locale = java.util.Locale.ENGLISH
  
  def writeFeatures( features: Seq[Feature], outFile: File ) {
    
    val out = new PrintWriter(new FileOutputStream(outFile))
    writeFeatures(features, out)
    out.close()
  }
  
  /**
  * Save a FeatureSet
  * @param featureSet
  * @param out
  */
  def writeFeatures( features: Seq[Feature], out: PrintWriter ) {
    
    // Write the header
    val colNames = List( "id","apex_scan","first_scan", "last_scan","elution_time","charge","moz","intensity_sum","area",
                         "quality_score","peakels_ratios","peakels_count","ms1_count","ms2_count","apex_ip","isotopic_patterns",
                         "overlap_factor","overlap_correlation","overlapping_feature" )
    
    val header = colNames.mkString("\t")
    out.println(header)
    out.flush()
    
    // Export the feature
    for( ft <- features ) {
      
      // Stringify isotopic patterns
      /*Object[] ipSrtings = new Object[ft.isotopicPatterns.length];
      
      int ipIndex = 0;
      for( IsotopicPattern ip: ft.isotopicPatterns ) {
        ipStrings[ipIndex] = ip.toJSONString();
        ipIndex++;
      }
      String ipString = "[" + StringUtils.join(ipStrings,",") + "]"; */
      //val ipString = generate(ft.getIsotopicPatterns)
      
      var overlapCorrelationStr = ""
      var bestOlpFtJSONString = ""
      val bestOlpFt = Option(ft.overlapProperties).map( _.getBestOverlappingFeature.feature ).orNull
      
      if( bestOlpFt!= null ) {
        overlapCorrelationStr = "%.8f".formatLocal( locale, bestOlpFt.overlapProperties.getOverlapPMCC )
        
        /*Object[] bestOlpIpSrings = new Object[bestOlpFt.isotopicPatterns.length];
        
        ipIndex = 0;
        for( IsotopicPattern ip: bestOlpFt.isotopicPatterns ) {
          bestOlpIpSrings[ipIndex] = ip.toJSONString();
          ipIndex++;
        }*/
        
        //bestOlpIpSring = "[" + StringUtils.join(bestOlpIpSrings,",") + "]"; 
        
        val olpFtJSONObject = new HashMap[String,Any]()
        olpFtJSONObject.put("id", bestOlpFt.id )
        olpFtJSONObject.put("apex_scan", bestOlpFt.getApexScanHeader.getInitialId )
        olpFtJSONObject.put("first_scan", bestOlpFt.getScanHeaders.head.getInitialId )
        olpFtJSONObject.put("last_scan", bestOlpFt.getScanHeaders.last.getInitialId )
        olpFtJSONObject.put("elution_time",bestOlpFt.getElutionTime)
        olpFtJSONObject.put("charge",bestOlpFt.charge)
        olpFtJSONObject.put("moz", "%.8f".formatLocal(locale, bestOlpFt.mz) )
        olpFtJSONObject.put("intensity_sum","%.1f".formatLocal(locale,bestOlpFt.getIntensitySum) )
        olpFtJSONObject.put("area","%.1f".formatLocal(locale,bestOlpFt.area) )
        olpFtJSONObject.put("quality_score", "%.8f".formatLocal(locale,Option(bestOlpFt.qualityProperties).map(_.getMeanPeakelCorrelation).getOrElse(0f) ) )
        olpFtJSONObject.put("ms1_count",bestOlpFt.getMs1Count)
        olpFtJSONObject.put("ms2_count",bestOlpFt.getMs2Count)
        //olpFtJSONObject.put("apex_ip",bestOlpFt.getIsotopicPatternAtApex() )
        //olpFtJSONObject.put("isotopic_patterns", bestOlpFt.getIsotopicPatterns() )
        
        //bestOlpFtJSONString = generate(olpFtJSONObject)
      }
      
      // Stringify feature
      // TODO: replace getMeanPeakelCorrelation by quality score
      val peakelCorrel = Option(ft.qualityProperties).map(_.getMeanPeakelCorrelation).getOrElse(0f)
      var qualityScoreStr = ""
      if( peakelCorrel.isNaN == false ) { qualityScoreStr = "%.8f".formatLocal(locale,peakelCorrel) }
      
      var peakelsRatiosStr = ""
      if( ft.getPeakelsCount > 1 ) {
        val peakelsRatiosOpt = ft.calcPeakelsAreaRatios
        if( peakelsRatiosOpt.isDefined ) {
          val formattedPeakelsRatios = peakelsRatiosOpt.get.map { "%.2f".formatLocal(locale,_) }        
          //peakelsRatiosStr = generate(formattedPeakelsRatios)
        }
      }
      
      val ftValues = List(
        ft.id,
        ft.getApexScanHeader.getInitialId,
        ft.getScanHeaders.head.getInitialId,
        ft.getScanHeaders.last.getInitialId,
        ft.getElutionTime,
        ft.charge,
        "%.8f".formatLocal(locale,ft.mz),
        "%.1f".formatLocal(locale,ft.getIntensitySum),
        "%.1f".formatLocal(locale,ft.area),
        qualityScoreStr,
        peakelsRatiosStr,
        ft.getPeakelsCount,
        ft.getMs1Count,
        ft.getMs2Count,
        //generate( ft.getIsotopicPatternAtApex ),
        "",//ipString,
        "%.2f".formatLocal(locale,Option(ft.overlapProperties).map(_.getOverlapRelativeFactor).getOrElse(0f) ),
        overlapCorrelationStr,
        bestOlpFtJSONString
      )
      
      val ftString = ftValues.mkString("\t")
      
      out.println( ftString )
      out.flush()
    }
    
    out.flush()
    out.close()
    
  }

}