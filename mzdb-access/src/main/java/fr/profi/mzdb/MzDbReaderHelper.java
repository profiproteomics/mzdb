package fr.profi.mzdb;

import com.almworks.sqlite4java.SQLiteException;
import java.io.File;
import java.util.concurrent.Callable;

import fr.profi.mzdb.io.reader.cache.MzDbEntityCache;
import fr.profi.mzdb.model.Spectrum;
import fr.profi.mzdb.model.SpectrumData;
import fr.profi.mzdb.model.SpectrumHeader;
import fr.profi.mzdb.model.SpectrumSlice;
import fr.profi.mzdb.util.concurrent.Callback;
import java.io.FileNotFoundException;
import java.io.StreamCorruptedException;
import java.util.logging.Level;
import java.util.logging.Logger;

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
    public static Callable<SpectrumSlice[]> getSpectrumSlicesInRanges(
            final double minMz,
            final double maxMz,
            final float minRt,
            final float maxRt,
            final File file,
            final MzDbEntityCache cache,
            final Callback<SpectrumSlice[]> callback) {

        return new Callable<SpectrumSlice[]>() {

            @Override
            public SpectrumSlice[] call() throws Exception {
                MzDbReader reader = new MzDbReader(file, cache, false);
                SpectrumSlice[] msSpectrumSlices = reader.getMsSpectrumSlices(minMz, maxMz, minRt, maxRt);
                if (callback != null) {
                    callback.onCompletion(msSpectrumSlices);
                }
                return msSpectrumSlices;
            }
        };

    }

    public static boolean isValid(File file) {

        boolean pass = false;

        /*
         boolean pass = false;
         MzDbReader reader = null;
         try {

         reader = new MzDbReader(file, true);

         SpectrumHeader[] ms1Headers = reader.getMs1SpectrumHeaders();

         SpectrumHeader[] ms2Headers = reader.getMs2SpectrumHeaders();

         System.out.println("skata");
            
         if (ms1Headers != null && ms1Headers.length > 0 && ms2Headers != null && ms2Headers.length > 0) {
         Spectrum rawSpectrum = reader.getSpectrum(ms1Headers[0].getId());

         if (rawSpectrum != null) {

         SpectrumData data = rawSpectrum.getData();
         if (data != null) {
         final double[] mzList = data.getMzList();
         if (mzList != null && mzList.length > 0) {
         pass = true;
         } else {
         pass = false;
         }
         } else {
         pass = false;
         }
         } else {
         pass = false;
         }
         }

         } catch (ClassNotFoundException | FileNotFoundException | SQLiteException e) {
         pass = false;
         } catch (StreamCorruptedException ex) {
         pass = false;
         } finally {
         if (reader != null) {
         reader.close();
         }
         }
         return pass;
         */
        MzDbReader reader = null;

        try {
            reader = new MzDbReader(file, true);

            SpectrumHeader[] headers = reader.getMs2SpectrumHeaders();

            if (headers != null && headers.length > 0) {
                
                Spectrum rawSpectrum = reader.getSpectrum(headers[0].getId());

                if (rawSpectrum != null) {

                    SpectrumData data = rawSpectrum.getData();
                    
                    if (data != null) {
                        
                        final double[] mzList = data.getMzList();
                        
                        if (mzList != null && mzList.length > 0) {
                            pass = true;
                        } else {
                            pass = false;
                        }
                        
                    } else {
                        pass = false;
                    }
                    
                } else {
                    pass = false;
                }
                
            }

        } catch (ClassNotFoundException | FileNotFoundException | SQLiteException e) {
            return false;
        } catch (StreamCorruptedException ex) {
            Logger.getLogger(MzDbReaderHelper.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            if (reader != null) {
                reader.close();
            }
        }

        return pass;

    }

}
