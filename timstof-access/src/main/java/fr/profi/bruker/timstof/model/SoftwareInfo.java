package fr.profi.bruker.timstof.model;

public class SoftwareInfo {

    private String m_version;
    private String m_name;

    public SoftwareInfo(String name,String version){
        m_name=name;
        m_version=version;
    }

    public String getName(){
        return m_name;
    }

    public String getVersion(){
        return m_version;
    }

}
