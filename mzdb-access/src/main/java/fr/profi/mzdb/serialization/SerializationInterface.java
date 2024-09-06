package fr.profi.mzdb.serialization;

import java.io.IOException;

public interface SerializationInterface {

    void write(SerializationWriter writer) throws IOException;

    void read(SerializationReader reader) throws IOException;

}
