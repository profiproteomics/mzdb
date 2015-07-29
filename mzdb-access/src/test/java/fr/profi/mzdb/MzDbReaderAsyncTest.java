package fr.profi.mzdb;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

import org.junit.Assert;
import org.junit.Test;

import com.almworks.sqlite4java.SQLite;
import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.io.reader.MzDbEntityCache;
import fr.profi.mzdb.model.ScanSlice;
import fr.profi.mzdb.utils.concurrent.Callback;

public class MzDbReaderAsyncTest implements Callback<ScanSlice[]> {

    private int callbackCount = 0;
    private static final URL filename_OVEMB150205_12 = MzDbReaderAsyncTest.class
	    .getResource("/OVEMB150205_12.raw.0.9.7.mzDB");

    static {
	try {
	    System.err.println("SQLite version : " + SQLite.getSQLiteVersion() + " #"
		    + SQLite.getSQLiteVersionNumber() + " lib #" + SQLite.getLibraryVersion());
	    System.err.println("SQLite compilation options : " + SQLite.getSQLiteCompileOptions());
	} catch (Exception e) {
	    System.err.println("SQLite library is not loaded");
	    System.err.println(
		    "if running test under Eclipse EDI, please add 'VM Argument' = '-Djava.library.path=/path/to/sqlite.library' in JUnit test run configuration");
	    System.err.println("for Windows OS library version is sqlite4java-win32-x64-1.0.392.dll");
	    System.err.println(
		    "in JeT's configuration path is 'D:\\Utilisateurs\\jturb\\.m2\\repository\\com\\almworks\\sqlite4java\\sqlite4java-win32-x64\\1.0.392'");
	}
    }

    /**
     * Non regression test date: jul 17th 2015
     *
     * @throws URISyntaxException
     * @throws SQLiteException
     * @throws FileNotFoundException
     * @throws ClassNotFoundException
     */
    @Test
    public void readerAsyncTest_OVEMB150205_12()
	    throws URISyntaxException, ClassNotFoundException, FileNotFoundException, SQLiteException {

	final MzDbEntityCache cache = new MzDbEntityCache();
	final double minMz = 00;
	final double maxMz = 1000;
	final float minRt = 100;
	final float maxRt = 10000;

	final File file_OVEMB150205_12 = new File(filename_OVEMB150205_12.toURI());
	// try {
	// MzDbReader mzDb = new MzDbReader(MzDbReaderAsyncTest.class.getResource(filename).getFile(), true);
	// ScanSlice[] scanSlices1 = mzDb.getMsScanSlicesAsync(minMz, maxMz, minRt, maxRt, null).get();
	// System.out.println(scanSlices1.length);
	// } catch (InterruptedException | ExecutionException | StreamCorruptedException | SQLiteException
	// | ClassNotFoundException | FileNotFoundException e) {
	// e.printStackTrace();
	// Assert.fail("MzDB reader instantiation exception " + e.getMessage() + " for " + filename);
	// }
	// create Reader
	Assert.assertEquals(0, this.callbackCount);
	ExecutorService executor = Executors.newFixedThreadPool(5);
	try {
	    // create reader in main thread
	    Assert.assertTrue("file does not exist", file_OVEMB150205_12.isFile());
	    MzDbReader mzDb = new MzDbReader(file_OVEMB150205_12, true);
	    Assert.assertNotNull("invalid file", mzDb);

	    // create a thread and launch a request
	    FutureTask<ScanSlice[]> futureTask0 = new FutureTask<ScanSlice[]>(new Callable<ScanSlice[]>() {

		@Override
		public ScanSlice[] call() throws Exception {
		    System.out.println("file = " + file_OVEMB150205_12);
		    MzDbReader mzDb = new MzDbReader(file_OVEMB150205_12, true);
		    System.out.println("new reader created");
		    return mzDb.getMsScanSlices(minMz, maxMz, minRt, maxRt);
		}
	    });
	    executor.execute(futureTask0);
	    ScanSlice[] scanSlices0 = futureTask0.get();
	    Assert.assertEquals("invalid number of slices", 80, scanSlices0.length);
	    Assert.assertEquals(0, this.callbackCount);

	    // use helper method to launch a request
	    Future<ScanSlice[]> scanSlicesFuture1 = Executors.newSingleThreadExecutor()
		    .submit(MzDbReaderHelper.getScanSlicesInRanges(minMz, maxMz, minRt, maxRt,
			    file_OVEMB150205_12, cache, this));

	    ScanSlice[] scanSlices1 = scanSlicesFuture1.get();
	    Assert.assertEquals(1, this.callbackCount);

	    Assert.assertEquals("invalid number of slices", 80, scanSlices1.length);

	    // launch two request at the same time
	    FutureTask<ScanSlice[]> futureTask2 = new FutureTask<ScanSlice[]>(new Callable<ScanSlice[]>() {

		@Override
		public ScanSlice[] call() throws Exception {
		    MzDbReader mzDb = new MzDbReader(file_OVEMB150205_12, true);
		    System.out.println("new reader created");
		    return mzDb.getMsScanSlices(minMz, maxMz, minRt, maxRt);
		}
	    });
	    FutureTask<ScanSlice[]> futureTask3 = new FutureTask<ScanSlice[]>(new Callable<ScanSlice[]>() {

		@Override
		public ScanSlice[] call() throws Exception {
		    MzDbReader mzDb = new MzDbReader(file_OVEMB150205_12, true);
		    System.out.println("new reader created");
		    return mzDb.getMsScanSlices(minMz, maxMz, minRt, maxRt);
		}
	    });
	    Assert.assertEquals(1, this.callbackCount);
	    executor.execute(futureTask2);
	    executor.execute(futureTask3);

	    ScanSlice[] scanSlices2 = futureTask2.get();
	    ScanSlice[] scanSlices3 = futureTask3.get();
	    Assert.assertEquals(1, this.callbackCount);
	    Assert.assertEquals("invalid number of slices", 80, scanSlices2.length);
	    Assert.assertEquals("invalid number of slices", 80, scanSlices3.length);

	    Assert.assertEquals(1, this.callbackCount);
	    Future<ScanSlice[]> scanSlicesFuture4 = Executors.newSingleThreadExecutor()
		    .submit(MzDbReaderHelper.getScanSlicesInRanges(minMz, maxMz, minRt, maxRt,
			    file_OVEMB150205_12, cache, this));
	    ScanSlice[] scanSlices4 = scanSlicesFuture4.get();
	    Future<ScanSlice[]> scanSlicesFuture5 = Executors.newSingleThreadExecutor()
		    .submit(MzDbReaderHelper.getScanSlicesInRanges(minMz, maxMz, minRt, maxRt,
			    file_OVEMB150205_12, cache, this));
	    ScanSlice[] scanSlices5 = scanSlicesFuture5.get();

	    Assert.assertEquals("invalid number of slices", 80, scanSlices4.length);
	    Assert.assertEquals("invalid number of slices", 80, scanSlices5.length);
	    Assert.assertEquals(3, this.callbackCount);

	} catch (InterruptedException | ExecutionException e) {
	    e.printStackTrace();
	    Assert.fail("MzDB reader instantiation exception " + e.getMessage() + " for "
		    + file_OVEMB150205_12.getAbsolutePath());
	}

	System.out.print(".");

	System.out.println(" OK");
    }

    /*
     * (non-Javadoc)
     *
     * @see fr.profi.mzdb.utils.future.FutureCallback#onCompletion(java.lang.Object)
     */
    @Override
    public void onCompletion(ScanSlice[] result) {
	this.callbackCount++;

    }
}
