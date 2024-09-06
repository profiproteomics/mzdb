package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.UserTermTable;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

public class UserTerm implements SerializationInterface  {

    public static final String TABLE_NAME = UserTermTable.tableName;

    protected long id;

    protected String name;

    protected String unitAccession;

    protected String type;

    public UserTerm(SerializationReader reader) throws IOException {
        read(reader);
    }

    public UserTerm(long id, String name, String unitAccession, String type) {
        this.id = id;
        this.name = name;
        this.unitAccession = unitAccession;
        this.type = type;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getUnitAccession() {
        return unitAccession;
    }

    public void setUnitAccession(String unitAccession) {
        this.unitAccession = unitAccession;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @Override
    public void write(SerializationWriter writer) throws IOException {
        writer.writeInt64(id);
        writer.writeString(name);
        writer.writeString(type);

        boolean hasData = unitAccession!=null;
        writer.writeBoolean(hasData);
        if (hasData) {
            writer.writeString(unitAccession);
        }

    }

    @Override
    public void read(SerializationReader reader) throws IOException {

        id = reader.readInt64();
        name = reader.readString();
        type = reader.readString();
        boolean hasData = reader.readBoolean();
        if(hasData)
            unitAccession = reader.readString();

    }
}
