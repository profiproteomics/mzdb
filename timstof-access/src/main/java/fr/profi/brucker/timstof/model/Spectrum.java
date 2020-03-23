package fr.profi.brucker.timstof.model;

import java.util.Arrays;

public class Spectrum {

    private String m_title;
    private Integer index;
    private int msLevel;

    private float retentionTime;
    private double[] m_masses;
    private float[] m_intensities;

    public Spectrum(String title, int msLevel, float retentionTime, double[] masses, float[] intensities) {
        this.m_title = title;
        this.msLevel = msLevel;
        this.retentionTime = retentionTime;
        this.m_masses = masses;
        this.m_intensities = intensities;
    }


    void  addPeaks(double[] masses, float[] intensities){
        if(masses.length!=intensities.length)
            throw  new IllegalArgumentException("Specified masses and intensities array must have same length ");
        double[] newMasses = Arrays.copyOf(m_masses, m_masses.length + masses.length);
        float[] newIntensities = Arrays.copyOf(m_intensities, m_intensities.length + intensities.length);

        for(int i = m_masses.length; i<newMasses.length;i++){
            newMasses[i] = masses[i-m_masses.length];
            newIntensities[i] = intensities[i-m_masses.length];
        }
        m_masses = newMasses;
        m_intensities = newIntensities;
    }

    public double[] getMasses(){
        return Arrays.copyOf(m_masses, m_masses.length);
    }

    public float[] getIntensities(){
        return Arrays.copyOf(m_intensities, m_intensities.length);
    }

    public String getTitle(){
        return  m_title;
    }
}
