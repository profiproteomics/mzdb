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

    //private XMLData m_XMLData;

    public MzdbWriterApi() {
        /*m_XMLData = new XMLData();*/
    }

    public String initializeMzdb(String path, MzDBMetaData mzDbMetaData, AcquisitionMode srcAcqMode) {

        try {

            //m_XMLData.mzDbMetaData =  mzDbMetaData;
            //m_XMLData.srcAcqMode =   srcAcqMode;


            boolean isDIA = (srcAcqMode != null && srcAcqMode.equals(fr.profi.mzdb.model.AcquisitionMode.SWATH));

            File destinationFile = new File(path);
            BBSizes defaultBBsize = new BBSizes(5, 10000, 15, 0);

            m_writer = new MzDBWriter(destinationFile, mzDbMetaData, defaultBBsize, isDIA);
            m_writer.initialize();
        } catch (Exception e) {
            LOGGER.error("error in initializeMzdb", e);
            return "KO:"+e.getMessage();
        }
        return "OK";
    }

    public String addspectrum(Spectrum spectrum, SpectrumMetaData spectrumMetaData, DataEncoding dataEncoding)  throws IOException, SQLiteException {

        try {

            /*m_XMLData.spectrum.add(spectrum);
            m_XMLData.spectrumMetaData.add(spectrumMetaData);
            m_XMLData.dataEncoding.add(dataEncoding);*/

            SpectrumHeader spectrumHeader = spectrum.getHeader();


            m_writer.insertSpectrum(spectrum, spectrumMetaData, dataEncoding);
        } catch (Exception e) {
            LOGGER.error("error in addspectrum", e);
            return "KO:"+e.getMessage();
        }

        return "OK";
    }

    public String closedb() {

/*try {

    JAXBContext contextObj = JAXBContext.newInstance(XMLData.class);

    Marshaller marshallerObj = contextObj.createMarshaller();
    marshallerObj.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

    marshallerObj.marshal(new JAXBElement<XMLData>(new QName("", "XMLData"), XMLData.class, null, m_XMLData), new FileOutputStream("./mzDbMetaData1.xml"));
} catch (Exception e) {
    e.printStackTrace();
}*/

        if (m_writer != null) {
            m_writer.close();
            return "OK";
        }
        LOGGER.error("error in closedb");
        return "KO";

    }


    /*public static class XMLData {
        public MzDBMetaData mzDbMetaData;
        public AcquisitionMode srcAcqMode;

        public ArrayList<Spectrum> spectrum = new ArrayList<>();
        public ArrayList<SpectrumMetaData> spectrumMetaData = new ArrayList<>();
        public ArrayList<DataEncoding>  dataEncoding = new ArrayList<>();
    }*/

}
