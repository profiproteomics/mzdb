package fr.profi.mzdb.model;

// TODO: Auto-generated Javadoc

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

/**
 * The Class Spectrum.
 * 
 * @author David Bouyssie
 */
public class Spectrum implements SerializationInterface {

	/** The header. */
	protected SpectrumHeader header;

	/** The spectrum data. */
	protected SpectrumData spectrumData;

	public Spectrum(SerializationReader reader) throws IOException {
		read(reader);
	}

	/**
	 * Instantiates a new spectrum.
	 * 
	 * @param header
	 *            the header
	 * @param spectrumData
	 *            the spectrum data
	 */
	public Spectrum(SpectrumHeader header, SpectrumData spectrumData) {
		super();
		this.header = header;
		this.spectrumData = spectrumData;
	}

	/**
	 * Gets the header.
	 * 
	 * @return the header
	 */
	public SpectrumHeader getHeader() {
		return this.header;
	}

	/**
	 * Gets the data.
	 * 
	 * @return the data
	 */
	public SpectrumData getData() {
		return this.spectrumData;
	}

	/**
	 * Gets the peaks.
	 * 
	 * @return the peaks
	 */
	public Peak[] toPeaks() {
		return spectrumData.toPeaks(this.header);
	}
	
	public Peak getNearestPeak(double mz, double mzTolPPM) {
		return this.spectrumData.getNearestPeak(mz, mzTolPPM, header);
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		header.write(writer);
		spectrumData.write(writer);
	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		header = new SpectrumHeader(reader);

		spectrumData = new SpectrumData(reader);
	}
}
