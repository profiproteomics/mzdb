package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;

import java.io.IOException;

public class SelectedIon extends AbstractParamTree {

    public SelectedIon() {}

    public SelectedIon(SerializationReader reader) throws IOException {
        read(reader);
    }
}
