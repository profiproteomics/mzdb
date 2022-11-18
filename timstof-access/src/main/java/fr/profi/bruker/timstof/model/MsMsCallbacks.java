package fr.profi.bruker.timstof.model;

import com.sun.jna.Pointer;
import fr.profi.bruker.timstof.TDFLibrary;

import java.util.HashMap;
import java.util.Map;

public class MsMsCallbacks {

    public static class MsMsData implements TDFLibrary.MsMsCallback {

        long precursorId = 0;
        int numPeaks = 0;
        double[] mz_values;
        float[] intensity_values;

        @Override
        public void invoke(long precursor_id, int num_peaks, Pointer pMz, Pointer pIntensites) {
            precursorId = precursor_id;
            numPeaks = num_peaks;
            mz_values = pMz.getDoubleArray(0, num_peaks);
            intensity_values = pIntensites.getFloatArray(0, num_peaks);
        }
    }

    public static class MultipleMsMsData implements TDFLibrary.MsMsCallback {

        Map<Long, MsMsSpectrum> msmsSpectraByPrecursorId = new HashMap<>();

        @Override
        public void invoke(long precursor_id, int num_peaks, Pointer pMz, Pointer pIntensites) {
            MsMsSpectrum msMsSpectrum = new MsMsSpectrum();
            msMsSpectrum.nbPeaks= num_peaks;
            msMsSpectrum.mz_values  = pMz.getDoubleArray(0, num_peaks);
            msMsSpectrum.intensity_values = pIntensites.getFloatArray(0, num_peaks);
            msmsSpectraByPrecursorId.put(precursor_id, msMsSpectrum);
        }

        public Map<Long, MsMsSpectrum> getMsMsSpectra(){
            return  new HashMap<>(msmsSpectraByPrecursorId);
        }
    }


    public static  class MsMsSpectrum {
        public double[] mz_values;
        public float[] intensity_values;
        public int nbPeaks;
    }
}