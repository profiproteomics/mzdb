package fr.profi.mzdb.client;

import fr.profi.mzdb.model.*;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.*;
import java.net.Socket;
import java.net.UnknownHostException;

public class MzdbClient {

    private SerializationWriter m_writer;

    private SerializationReader m_reader;

    public MzdbClient() {
    }

    public void connect(String host, int port) throws Exception {
        Socket socket = null;
        DataOutputStream sockDataOut = null;
        BufferedReader sockIn = null;

        try {
            socket = new Socket(host, port);
            DataOutputStream outputStream = new DataOutputStream(socket.getOutputStream());
            m_writer = new SerializationWriter(outputStream, 4096);
            m_reader = new SerializationReader(socket.getInputStream());
        } catch (UnknownHostException e) {
            throw new Exception("hote non atteignable : "+host+"\n"+e.getMessage());
        } catch (IOException e) {
            throw new Exception("connection impossible avec : "+host+"\n"+e.getMessage());
        }

    }

    public String createMzdb(String path, AcquisitionMode srcAcqMode ) throws Exception {

        m_writer.writeInt32(MethodKeys.METHOD_KEY_INITIALIZE_MZDB);
        m_writer.writeString(path);
        //mzDbMetaData.write(m_writer);
        srcAcqMode.write(m_writer);
        m_writer.flush();

        return readReturnedString();

    }

    public String addMzdbMetaData(MzDBMetaData mzDbMetaData) throws Exception {
        m_writer.writeInt32(MethodKeys.METHOD_KEY_ADD_MZDB_METADATA);
        mzDbMetaData.write(m_writer);
        m_writer.flush();

        return readReturnedString();

    }

    public String addSpectrum(Spectrum spectrum /*, SpectrumMetaData spectrumMetaData*/, DataEncoding dataEncoding) throws Exception {


        m_writer.writeInt32(MethodKeys.METHOD_KEY_ADD_SPECTRUM);
        spectrum.write(m_writer);
        //spectrumMetaData.write(m_writer); // now spectrum must contains data of sprectrumMetadata
        dataEncoding.write(m_writer);
        m_writer.flush();

        return readReturnedString();
    }

    public String closedb() throws Exception {
        m_writer.writeInt32(MethodKeys.METHOD_KEY_CLOSE_MZDB);
        m_writer.flush();

        return readReturnedString();
    }

    public void exitServer() throws Exception {
        m_writer.writeInt32(MethodKeys.METHOD_KEY_EXIT);
        m_writer.flush();
    }

    private String readReturnedString() throws IOException {
        return m_reader.readString();

    }


}
