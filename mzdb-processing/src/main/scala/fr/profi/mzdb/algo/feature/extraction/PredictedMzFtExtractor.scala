package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader

class PredictedMzFtExtractor(
  val mzDbReader: MzDbReader,
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val mzTolPPM: Float,
  val maxNbPeaksInIP: Int,
  val minNbOverlappingIPs: Int
) extends ISupervisedFtExtractor {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
    
    // Retrieve a list of sorted scan initial ids
    
    //ArrayList<Integer> sortedScanIds = (ArrayList<Integer>) Ordering.from(this.sortIntegersAsc).sortedCopy( pklTree.pklGroupByScanId.keySet() );
    //ArrayList<IsotopicPattern> ips = new ArrayList<IsotopicPattern>( sortedScanIds.size() );
  
    Option.empty[Feature]
  }

}
