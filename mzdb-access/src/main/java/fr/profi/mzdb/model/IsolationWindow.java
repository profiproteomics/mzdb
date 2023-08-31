package fr.profi.mzdb.model;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

/**
 * 
 * 
 */
public class IsolationWindow implements SerializationInterface {
    
    private double minMz;
    private double maxMz;

	public IsolationWindow(SerializationReader reader) throws IOException {
		read(reader);
	}

    public IsolationWindow(double minMz, double maxMz) {
			this.minMz = minMz;
			this.maxMz = maxMz;
    }
    
    public double getMinMz() {
	return this.minMz;
    }
    
    public double getMaxMz() {
	return this.maxMz;
    }

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(maxMz);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(minMz);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		IsolationWindow other = (IsolationWindow) obj;
		if (Double.doubleToLongBits(maxMz) != Double.doubleToLongBits(other.maxMz))
			return false;
		if (Double.doubleToLongBits(minMz) != Double.doubleToLongBits(other.minMz))
			return false;
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "IsolationWindow [minMz=" + minMz + ", maxMz=" + maxMz + "]";
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeDouble(minMz);
		writer.writeDouble(maxMz);

	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		minMz = reader.readDouble();
		maxMz = reader.readDouble();
	}
}
