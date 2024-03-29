package fr.profi.bruker.timstof.io;

import fr.profi.bruker.timstof.model.*;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntList;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Paths;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TDFMetadataReader {

    private static Logger LOG = LoggerFactory.getLogger(TDFMetadataReader.class);
    private Int2ObjectMap<Precursor> m_allPrecursors;
    private File m_ttDirFile;
    static {
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            throw new RuntimeException("Can't read tilmstof Metadata:  unable to load driver for SQLIte database.");
        }
    }

    protected TDFMetadataReader(File timsTofDataDir) {
        m_ttDirFile = timsTofDataDir;
    }

    int getFrameCount(){
        Connection connection;
        try {

            String tdf_path = Paths.get(m_ttDirFile.getAbsolutePath(), "analysis.tdf").toString();
            connection = DriverManager.getConnection("jdbc:sqlite:" + tdf_path);

            Statement statement = connection.createStatement();
            statement.setQueryTimeout(30); // set timeout to 30 sec.

            // Example: read frame parameter
            ResultSet rsFrames = statement.executeQuery("SELECT COUNT(*) FROM Frames");
            rsFrames.next();
            int numFrames = rsFrames.getInt(1);
            rsFrames.close();
            LOG.debug("number of TIMS frames: "+ numFrames);
            return  numFrames;
        }catch (SQLException e) {
            // the sqlite jdbc driver has a strange behaviour when the sqlite file cannot be found: It throws an exception with
            // message: [SQLITE_CANTOPEN]  Unable to open the database file (out of memory) ...
            // so if you get that exception check your path, filename, rights ...
            LOG.error(e.getMessage());
            return -1;
        }
    }

    Int2ObjectMap<Precursor> getPrecursorInfoById (){
        if(m_allPrecursors == null)
            readPrecursorInfo();
        return m_allPrecursors;
    }

    private  void readPrecursorInfo(){
        if(m_allPrecursors == null) {
            Connection connection = getConnection();
            m_allPrecursors = new Int2ObjectOpenHashMap<>();
            try {
                if (connection != null) {
                    Statement statement = connection.createStatement();
                    statement.setQueryTimeout(30); // set timeout to 30 sec.

                    ResultSet rsFrames = statement.executeQuery("SELECT Id, LargestPeakMz, AverageMz, MonoisotopicMz, Charge, ScanNumber, Intensity, Parent FROM Precursors");
                    while (rsFrames.next()) {
                        int precId = rsFrames.getInt(1);
                        double largestPeakMz = rsFrames.getDouble(2);
                        double avgMz = rsFrames.getDouble(3);
                        double monoIsotopMz = rsFrames.getDouble(4);
                        int charge = rsFrames.getInt(5);
                        double scanNbr = rsFrames.getDouble(6);
                        double intensity = rsFrames.getDouble(7);
                        int parentFr = rsFrames.getInt(8);
                        Precursor prec = new Precursor(precId, largestPeakMz, avgMz, monoIsotopMz, charge, scanNbr, intensity, parentFr);
                        m_allPrecursors.put(precId, prec);
                    }
                    rsFrames.close();
                }
            } catch (SQLException e) {
                e.printStackTrace();
                LOG.error(e.getMessage());
            }
        }
    }

    List<AbstractTimsFrame> readFramesInfo(IntList frameIds){
        List<AbstractTimsFrame> frames = new ObjectArrayList<>();
        Connection connection =getConnection();
        try{
            if(connection != null) {

                StringBuilder sqlQuery = new StringBuilder("SELECT Id, NumScans, NumPeaks, ScanMode, MsMsType, MaxIntensity, SummedIntensities,  Time FROM Frames WHERE Id in (");
                for(int id : frameIds){
                    sqlQuery.append(id).append(",");
                }
                sqlQuery.replace(sqlQuery.length()-1, sqlQuery.length(),")");

                Statement statement = connection.createStatement();
                statement.setQueryTimeout(30); // set timeout to 30 sec.

                ResultSet rsFrames = statement.executeQuery(sqlQuery.toString());
                while (rsFrames.next()){
                    int frId = rsFrames.getInt(1);
                    int nbScans = rsFrames.getInt(2);
                    int nbPeak = rsFrames.getInt(3);
                    int scanMode = rsFrames.getInt(4);
                    int msmsType = rsFrames.getInt(5);
                    int maxInt = rsFrames.getInt(6);
                    int summedInt = rsFrames.getInt(7);
                    double time= rsFrames.getDouble(8);
                    AbstractTimsFrame.MsMsType msmsTypeVal = AbstractTimsFrame.MsMsType.findByCode(msmsType);
                    AbstractTimsFrame frame = null;
                    switch (msmsTypeVal){
                        case PASEF:
                            frame = new TimsPASEFFrame(frId, nbScans, nbPeak, scanMode, maxInt, summedInt, time);
                            break;
                        case MS:
                            frame = new TimsMSFrame(frId, nbScans, nbPeak, scanMode, maxInt, summedInt, time);
                            break;
                    }
                    if(frame != null)
                        frames.add(frame);
                }
                rsFrames.close();
            }
            return frames;
        }catch (SQLException e) {
            // the sqlite jdbc driver has a strange behaviour when the sqlite file cannot be found: It throws an exception with
            // message: [SQLITE_CANTOPEN]  Unable to open the database file (out of memory) ...
            // so if you get that exception check your path, filename, rights ...
            LOG.error(e.getMessage());
            return frames;
        }
    }

   void readPasefMsMsInfo(List<TimsPASEFFrame> frames){

        Int2ObjectMap<List<PasefMsMsData>> pasefMsMsInfoByFrameId = new Int2ObjectOpenHashMap<>();
        Int2ObjectMap<AbstractTimsFrame> framesById = new Int2ObjectOpenHashMap<>();
        StringBuilder sb = new StringBuilder();
        for(int i=0; i<frames.size(); i++){
            int fid = frames.get(i).getId();
            framesById.put(fid, frames.get(i));
            sb.append(fid);
            if(i < (frames.size()-1))
                sb.append(',');
        }
        String frIdsAsStr = sb.toString();
        this.readPrecursorInfo();
        Connection connection =getConnection();
        try{
            if(connection != null) {

                Statement statement = connection.createStatement();
                statement.setQueryTimeout(30); // set timeout to 30 sec.

                ResultSet rsFrames = statement.executeQuery("SELECT Frame,  ScanNumBegin, ScanNumEnd, IsolationMz, IsolationWidth, CollisionEnergy,  Precursor FROM PasefFrameMsMsInfo WHERE Frame in (" + frIdsAsStr + ")");
                while (rsFrames.next()){
                    int frId = rsFrames.getInt(1);
                    int startScan = rsFrames.getInt(2);
                    int endScan = rsFrames.getInt(3);
                    double isolationmz = rsFrames.getDouble(4);
                    double isolationWidth = rsFrames.getDouble(5);
                    double collisionEnergy = rsFrames.getDouble(6);
                    int precursorId = rsFrames.getInt(7);
                    PasefMsMsData msmsInfo = new PasefMsMsData(frId, startScan, endScan, isolationmz, isolationWidth,collisionEnergy, precursorId, (float)framesById.get(frId).getTime());
                    msmsInfo.setPrecursor(m_allPrecursors.get(precursorId));
                    List<PasefMsMsData> frameMsMsInfos = pasefMsMsInfoByFrameId.getOrDefault(frId, new ArrayList<>());
                    frameMsMsInfos.add(msmsInfo);
                    pasefMsMsInfoByFrameId.put(frId, frameMsMsInfos);
                }
                rsFrames.close();
                for(TimsPASEFFrame fr : frames){
                    fr.setPasefMsMsData(pasefMsMsInfoByFrameId.getOrDefault(fr.getId(), new ArrayList<>()));
                }
            }
        }catch (SQLException e) {
            // the sqlite jdbc driver has a strange behaviour when the sqlite file cannot be found: It throws an exception with
            // message: [SQLITE_CANTOPEN]  Unable to open the database file (out of memory) ...
            // so if you get that exception check your path, filename, rights ...
            LOG.error(e.getMessage());
        }
    }

    public Map<String, String> readGlobalMetaData() {
        Connection connection =getConnection();
        Map<String,String> globalProperties = new HashMap<>();
        try{
            if(connection != null) {
                Statement statement = connection.createStatement();
                statement.setQueryTimeout(30); // set timeout to 30 sec.

                ResultSet rsGlobalMD = statement.executeQuery("SELECT Key, Value FROM GlobalMetadata");
                while (rsGlobalMD.next()){
                    String key = rsGlobalMD.getString(1);
                    String val = rsGlobalMD.getString(2);
                    globalProperties.put(key,val);
                }
            }
            return globalProperties;
        } catch (SQLException e) {
            // the sqlite jdbc driver has a strange behaviour when the sqlite file cannot be found: It throws an exception with
            // message: [SQLITE_CANTOPEN]  Unable to open the database file (out of memory) ...
            // so if you get that exception check your path, filename, rights ...
            LOG.error(e.getMessage());
            return globalProperties;
        }
    }


    public Map<String, String> readPropertyGroup(int groupId) {
        Connection connection = getConnection();
        Map<String,String> groupProperties = new HashMap<>();
        try{
            if(connection != null) {
                Statement statement = connection.createStatement();
                statement.setQueryTimeout(30); // set timeout to 30 sec.

                ResultSet rsGlobalMD = statement.executeQuery("SELECT Property, Value, PermanentName FROM GroupProperties gp, PropertyDefinitions pd where gp.Property = pd.Id AND gp.PropertyGroup = "+Integer.toString(groupId));
                while (rsGlobalMD.next()){
                    Integer id = rsGlobalMD.getInt(1);
                    String val = rsGlobalMD.getString(2);
                    String name = rsGlobalMD.getString(3);
                    groupProperties.put(name,val);
                }
            }
            return groupProperties;
        } catch (SQLException e) {
            LOG.error(e.getMessage());
            return groupProperties;
        }
    }

    public Map<String, String>  readFrameProperty(int frameId) {
        Connection connection = getConnection();
        Map<String,String> frameProperties = new HashMap<>();
        try{
            if(connection != null) {
                Statement statement = connection.createStatement();
                statement.setQueryTimeout(30); // set timeout to 30 sec.

                ResultSet rsGlobalMD = statement.executeQuery("SELECT PermanentName, Value FROM FrameProperties fp, PropertyDefinitions pd where fp.Property = pd.Id AND fp.Frame = "+Integer.toString(frameId));
                while (rsGlobalMD.next()){
                    String name = rsGlobalMD.getString(1);
                    String val = rsGlobalMD.getString(2);
                    frameProperties.put(name, val);
                }
            }
            return frameProperties;
        } catch (SQLException e) {
            LOG.error(e.getMessage());
            return frameProperties;
        }
    }

    private Connection getConnection(){
        Connection connection;
        try {
            String tdf_path = Paths.get(m_ttDirFile.getAbsolutePath(), "analysis.tdf").toString();
            connection = DriverManager.getConnection("jdbc:sqlite:" + tdf_path);
            return connection;
        }catch (SQLException e) {
            // the sqlite jdbc driver has a strange behaviour when the sqlite file cannot be found: It throws an exception with
            // message: [SQLITE_CANTOPEN]  Unable to open the database file (out of memory) ...
            // so if you get that exception check your path, filename, rights ...
            LOG.error(e.getMessage());
            return null;
        }

    }

}
