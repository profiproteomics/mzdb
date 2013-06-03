package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
             
class FullySupervisedFtExtractor(
  //val mzDbReader: MzDbReader,
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val mzTolPPM: Float,
  val maxNbPeaksInIP: Int,
  val minNbOverlappingIPs: Int
) extends AbstractSupervisedFtExtractor {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
    
    // Retrieve a list of sorted scan ids
    // TODO: check if cycles are a better option
    val sortedScanIds = putativeFt.firstScanId to putativeFt.lastScanId
    
    val theoIP = putativeFt.theoreticalIP
    val ips = new ArrayBuffer[IsotopicPattern]( sortedScanIds.length )
      
    // Iterate over scan ids sorted in an ascendant way
    for( scanId <- sortedScanIds ) {
      
      // Try to retrive the scan header
      for( curScanH <- this.scanHeaderById.get(scanId) ) {
        //println(curScanH.msLevel);
        
        val ipOpt = pklTree.extractIsotopicPattern( curScanH, theoIP, mzTolPPM, 2 )
        
        // Check if an isotopic pattern has been found
        if( ipOpt != None  ) {
          val ip = ipOpt.get
          //ip.elutionTime = curScanH.time;
          
          if( ip.peaks.length > 0 ) {
            
            val olpIPs = this._extractOverlappingIPs( ip, theoIP, pklTree )
            
            // Set overlapping IPs if at least one has been found
            val nbOlpIPs = olpIPs.length
            if( nbOlpIPs > 0 ) {
              ip.setOverlappingIps( olpIPs.toArray )
            }
          }
            
          // Add current isotope pattern the IP list
          ips += ip
        }
        
        //println( "#ip peaks=" +  ip.peaks.size() );
        //println( "#peaks at same time=" +  peaksAtTime.size() ); 
      }
    }
    
    //println( "#ip=" +  ips.size() + " ; mz=" + putativeFt.mz );
    //println(putativeFt.id +";");
    
    // Append extracted feature to the existing list
    val ft = new Feature( putativeFt.id, putativeFt.mz, putativeFt.charge, ips.toArray )

    this.updateFtOverlappingFeatures( ft, ips, this.minNbOverlappingIPs )
    
    /*
    ArrayList<Feature> overlappingFeatures = ft.getOverlappingFeatures();
    
    if( overlappingFeatures != null & ft.intensity > 0) {
      
      Feature bestOlpFt = ft.getBestOverlappingFeature();
      double bestOlpFtInt = bestOlpFt.getPeakelIntensity( bestOlpFt.peakelsCount - 1 );
      double overlapRelativeFactor = bestOlpFtInt/ft.getPeakelIntensity( 0 );
      double overlapPMCC = bestOlpFt.getOverlapPMCC();
      
      // overlapPMCC == overlapPMCC check that we have a defined value (no NaN)        
      if( overlapRelativeFactor > 0.25 && overlapPMCC == overlapPMCC && overlapPMCC > 0.5 ) {
        for( IsotopicPattern ip: ft.isotopicPatterns ) {
          System.out.println( ip.toString() );
        }            
        
        System.out.println( "nb olp fts=" + overlappingFeatures.size() );
        System.out.println( "bestOverlappingFeature charge=" + bestOlpFt.charge );
        System.out.println( "bestOverlappingFeature intensity=" + bestOlpFtInt );
        System.out.println( "bestOverlappingFeature overlap factor=" + overlapRelativeFactor );
        System.out.println( "bestOverlappingFeature overlap correlation=" + overlapPMCC );
        
        for( IsotopicPattern ip: bestOlpFt.isotopicPatterns ) {
          System.out.println( ip.toString() );
        }
               
        
        System.out.println( "***********************************************");
        
      
      }       

    }*/
  
    Some(ft)
  }
}