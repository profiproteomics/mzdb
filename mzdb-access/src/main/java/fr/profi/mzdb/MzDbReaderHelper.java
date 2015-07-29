/**
 *
 */
package fr.profi.mzdb;

import java.io.File;
import java.util.concurrent.Callable;

import fr.profi.mzdb.io.reader.MzDbEntityCache;
import fr.profi.mzdb.model.ScanSlice;
import fr.profi.mzdb.utils.concurrent.Callback;

/**
 * @author JeT
 *
 */
public class MzDbReaderHelper {

    private MzDbReaderHelper() {
	// helper class
    }

    /**
     * get an async
     *
     * @param minMz
     * @param maxMz
     * @param minRt
     * @param maxRt
     * @param file
     * @param cache
     * @param callback
     * @return
     */
    public static Callable<ScanSlice[]> getScanSlicesInRanges(final double minMz, final double maxMz,
	    final float minRt, final float maxRt, final File file, final MzDbEntityCache cache,
	    final Callback<ScanSlice[]> callback) {
	return new Callable<ScanSlice[]>() {

	    @Override
	    public ScanSlice[] call() throws Exception {
		MzDbReader reader = new MzDbReader(file, cache, false);
		ScanSlice[] msScanSlices = reader.getMsScanSlices(minMz, maxMz, minRt, maxRt);
		if (callback != null) {
		    callback.onCompletion(msScanSlices);
		}
		return msScanSlices;
	    }
	};

    }
}
