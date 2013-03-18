

import fr.profi.mzdb.MzDbReader
import java.io.File
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Scan
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.MzDbFeatureExtractor
import fr.profi.mzdb.io.reader.RunSliceDataProvider

object Extractor extends App {
  
		// Instantiate the mzDB
		val dbPath = "D:\\Utilisateurs\\Marc\\user_data\\Desktop\\QEFRD130122_11.raw.mzdb"
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
		extractor.extractFeatures(runSliceDataProvider, pf, 5)
}