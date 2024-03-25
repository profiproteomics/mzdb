package fr.profi.bruker.timstof;

import com.sun.jna.Callback;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

public interface TDFLibrary extends Library  {

    static TDFLibrary getInstance(String libFullPath ){
        TDFLibrary instance = (TDFLibrary) Native.load(libFullPath, TDFLibrary.class);
        return instance;
    }

    long tims_open(String analysis_dir, long use_recalib);
    long tims_close(long handle);
    long tims_get_last_error_string(byte[] error, long len);
    long tims_read_scans_v2(long handle, long frameId, long scanBegin, long scanEnd, byte[] scanBuffer, long len);
    long tims_index_to_mz(long handle, long frameId, double[] index, double[] mz, long len);
    long tims_scannum_to_oneoverk0(long handle, long frameId, double[] scannum, double[] oneOverK0, long len);

    interface MsMsCallback extends Callback {
        void invoke(long precursor_id, int num_peaks, Pointer pMz, Pointer pIntensites);
    }

    long tims_read_pasef_msms(long handle, long[] precursors, long num_precursors, MsMsCallback my_callback);
    long tims_read_pasef_msms_for_frame(long handle, long frameId, MsMsCallback my_callback);
}


