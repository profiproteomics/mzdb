package fr.profi.mzdb.client;

public class MethodKeys {

    public static final int METHOD_KEY_TEST = -1;

    // Generic Methods
    public static final int METHOD_KEY_FINISHED = 4;

    // -- Methods used to write data to mzdb --

    public static final int METHOD_KEY_INITIALIZE_MZDB = 0;
    public static final int METHOD_KEY_ADD_SPECTRUM = 1;
    public static final int METHOD_KEY_ADD_MZDB_METADATA = 2;
    public static final int METHOD_KEY_CLOSE_MZDB = 3;


    // -- Methods used to Read data and "CallBack" them
    public static final int METHOD_KEY_ADD_ACQ_METADATA = 5;
//    public static final int METHOD_KEY_END_READ = 6; // indicates data is read, client will be reset

}
