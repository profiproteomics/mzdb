package fr.profi.mzdb.client;

import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.BBSizes;
import fr.profi.mzdb.io.writer.MzDBWriter;
import fr.profi.mzdb.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

public class MzdbWriterApi {

    private static final Logger LOGGER = LoggerFactory.getLogger(MzdbWriterApi.class);

    private MzDBWriter m_writer = null;


    public MzdbWriterApi() {
    }

    public String initializeMzdb(String path, AcquisitionMode srcAcqMode) {

        try {
            boolean isDIA = (srcAcqMode != null && srcAcqMode.equals(fr.profi.mzdb.model.AcquisitionMode.SWATH));

            File destinationFile = new File(path);
            BBSizes defaultBBsize = new BBSizes(5, 10000, 15, 0);

            m_writer = new MzDBWriter(destinationFile, null, defaultBBsize, isDIA);
            m_writer.initialize();
        } catch (Exception e) {
            LOGGER.error("error in initializeMzdb", e);
            return "KO:"+e.getMessage();
        }
        return "OK";
    }

    public String addMzdbMetaData(MzDBMetaData mzDbMetaData) {

        try {
            m_writer.addMetaData(mzDbMetaData);
        } catch (Exception e) {
            LOGGER.error("error in addMzdbMetaData", e);
            return "KO:"+e.getMessage();
        }
        return "OK";
    }

    public String addspectrum(Spectrum spectrum/*, SpectrumMetaData spectrumMetaData*/, DataEncoding dataEncoding)  throws IOException, SQLiteException {

        try {
            m_writer.insertSpectrum(spectrum/*, spectrumMetaData*/, dataEncoding); // now spectrum must contains data of sprectrumMetadata
        } catch (Exception e) {
            LOGGER.error("error in addspectrum", e);
            return "KO:"+e.getMessage();
        }

        return "OK";
    }

    public String closedb() {


        if (m_writer != null) {
            m_writer.close();
            return "OK";
        }
        LOGGER.error("error in closedb");
        return "KO";

    }


}
