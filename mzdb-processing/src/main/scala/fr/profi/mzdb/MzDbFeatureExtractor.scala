package fr.profi.mzdb

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import collection.JavaConversions.mapAsScalaMap
import com.weiglewilczek.slf4s.Logging

import fr.profi.mzdb.algo.FeatureExtractor
import fr.profi.mzdb.algo.ms.normalization.MsScanNormalizer
import fr.profi.mzdb.io.reader.RunSliceDataProvider
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakList
import fr.profi.mzdb.model.RunSliceData
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.RunSliceHeader
import fr.profi.mzdb.model.PeakListGroup
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.ScanHeader

/**
 * @author David Bouyssie
 *
 */
class MzDbFeatureExtractor( mzDbReader: MzDbReader,
                            maxNbPeaksInIP: Int = 3,
                            minNbOverlappingIPs: Int = 3 ) extends Logging {
  
  class RichRunSliceData(self: RunSliceData) {
    def getPeakListByScanId(): Map[Int,PeakList] = {
      Map() ++ self.getScanSliceList.map { ss => ss.getScanId -> new PeakList(ss.getPeaks(),0.1) }
    }
  }
  implicit def rsdToRichRsd(rsd: RunSliceData) = new RichRunSliceData(rsd)

  def extractFeatures( rsdProvider: RunSliceDataProvider, putativeFeatures: Seq[PutativeFeature], mzTolPPM: Float ): Seq[Feature] = {

    // Retrieve run slices and map them by their number
    val rsHeaders = mzDbReader.getRunSliceHeaders(1)
    val rsHeaderByNumber = Map() ++ rsHeaders.map { rsh => rsh.getNumber -> rsh }
    if( rsHeaders.length != rsHeaderByNumber.size )
      throw new Exception("run slice headers must have a unique number")
    
    // Define a putative feature map
    val putativeFtsByRsNumber = new HashMap[Int, ArrayBuffer[PutativeFeature]]

    // Group putative features by run slice id
    for ( pft <- putativeFeatures) {      
      for ( rsh <- rsHeaders if rsh.getMsLevel == 1 && rsh.getBeginMz <= pft.mz && rsh.getEndMz > pft.mz ) {        
        putativeFtsByRsNumber.getOrElseUpdate( rsh.getId, new ArrayBuffer[PutativeFeature] ) += pft
      }
    }

    // Retrieve scans mapped by their initial id
    val scanHeadersById = Map() ++ mzDbReader.getScanHeaderById.map { case(i,sh) => i.toInt -> sh }

    // Compute MS scans normalization factors
    //val nfByScanId = MsScanNormalizer.computeNfByScanId(mzDbReader)
    
    // Define a peaklist map (first level = runSliceId, second level =scanId )
    val pklByScanIdAndRsId = new HashMap[Int, Map[Int, PeakList]]()

    // Define an array of features to be extracted
    val extractedFeatures = new ArrayBuffer[Feature](putativeFeatures.length)
  
    // Instantiate a feature extractor
    val ftExtractor = new FeatureExtractor( mzDbReader,
                                            scanHeadersById,
                                            null,//nfByScanId,
                                            mzTolPPM,
                                            maxNbPeaksInIP,
                                            minNbOverlappingIPs
                                           )

    // Iterate over run slice headers
    var( prevRSH, nextRSH ) = (Option.empty[RunSliceHeader], Option.empty[RunSliceHeader])
    
    for ( rsh <- rsHeaders if rsh.getMsLevel == 1 ) {
      this.logger.debug("processing run slice with id =" + rsh.getId)

      // Retrieve run slices and their corresponding id
      val rsNum = rsh.getNumber

      prevRSH = rsHeaderByNumber.get(rsNum - 1)
      val prevRsNumber = if( prevRSH == None ) 0 else prevRSH.get.getNumber

      nextRSH = rsHeaderByNumber.get(rsNum + 1)
      val nextRsNumber = if( nextRSH == None ) 0 else nextRSH.get.getNumber      
      
      // Build the list of obsolete run slices
      val runSlicesToRemove = for( processedRsId <- pklByScanIdAndRsId.keys
                                   if processedRsId != rsNum &&
                                      processedRsId != prevRsNumber &&
                                      processedRsId != nextRsNumber
                                 ) yield processedRsId

      // Clean the peaklist map => remove obsolete run slices
      runSlicesToRemove.foreach { pklByScanIdAndRsId -= _ }

      // Retrieve putative features corresponding to the current run slice
      val rsPutativeFts = putativeFtsByRsNumber.get(rsNum)
      
      if ( rsPutativeFts != None ) {

        // println("run slice id =" +runSlice.id +
        // " ; putative features count=" +
        // runSlicePutativeFeatures.size() );

        // Retrieve previous run slice peaklist
        if (prevRSH.isDefined) {
          if ( pklByScanIdAndRsId.contains(prevRsNumber) == false ) { 
            pklByScanIdAndRsId += ( prevRsNumber -> this._getRSD(rsdProvider,prevRsNumber).getPeakListByScanId )
          }
        }
  
        // Retrieve current run slice peakList
        if ( pklByScanIdAndRsId.contains(rsNum) == false ) {
          pklByScanIdAndRsId += ( rsNum -> this._getRSD(rsdProvider,rsNum).getPeakListByScanId )
        }
  
        // Retrieve current next slice peaklist
        if (nextRSH.isDefined) {
          if ( pklByScanIdAndRsId.contains(nextRsNumber) == false ) {
            pklByScanIdAndRsId += ( nextRsNumber -> this._getRSD(rsdProvider,nextRsNumber).getPeakListByScanId )
          }
        }
        
        // Group run slice peakLists into a single map (key = scan id)
        val peakListsByScanId = new HashMap[Int,ArrayBuffer[PeakList]]()
        pklByScanIdAndRsId.values.foreach { _.foreach { case (scanId, pkl) =>
          peakListsByScanId.getOrElseUpdate(scanId, new ArrayBuffer[PeakList]) += pkl
          }
        }
        
        // Use the map to instantiate a peakList tree which will be used for peak extraction
        val pklGroupByScanId = Map() ++ peakListsByScanId.map { kv => kv._1 -> new PeakListGroup( kv._2 ) }
        val pklTree = new PeakListTree( pklGroupByScanId )
  
        // Extract features using parallelization
        var error: Throwable = null
        
        val tmpXFts = rsPutativeFts.get.par.map { pft =>
          var xft = Option.empty[fr.profi.mzdb.model.Feature]
          
          try {
            xft = ftExtractor.extractFeature(pft, pklTree)
          } catch {
            case e => error = e
          }
          
          xft
        } toArray;
        
        if( error != null ) throw error        
        else for( xft <- tmpXFts if xft != None ) extractedFeatures += xft.get
        
      }
    }
    
    // TODO: add a boolean to disable this
    this.logger.info("nb features before apex redundancy detection:" + extractedFeatures.length )
    
    // TODO: try to group by the MS2 spectrum id nearest from the Apex
    val featuresByApex = extractedFeatures.groupBy( _.peakels(0).getApex )
    
    this.logger.info("nb features after apex redundancy detection:" + featuresByApex.size )
    
    val filteredFeatures = new ArrayBuffer[Feature](featuresByApex.size)
    
    for ( fts <- featuresByApex.values ) {
      // Sort duplicatedFts by descending maximum intensity
      val sortedFts = fts.sortBy( - _.peakels(0).duration )
      filteredFeatures += sortedFts(0)
    }
    
    filteredFeatures
    
  }
  
  private def _getRSD(rsdProvider: RunSliceDataProvider, rsNum: Int): RunSliceData = {
    val rsd = Option(rsdProvider.getRunSliceData(rsNum))
    if (rsd == None) {
      throw new Exception("run slice id '" + rsNum+ "' is out of range")
    }
    
    rsd.get
  }
  
}