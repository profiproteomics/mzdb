package fr.profi.mzdb.serialization;


import com.google.protobuf.CodedOutputStream;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

public class SerializationWriter {

    private CodedOutputStream codedOutput;

    public SerializationWriter(OutputStream outputStream, int bufferSize) {
        codedOutput = CodedOutputStream.newInstance(outputStream, bufferSize);
    }

    public void flush() throws IOException {
        codedOutput.flush();
    }

    // --- Base Types ---

    // String
    public void writeString(final String value) throws IOException {
        codedOutput.writeStringNoTag(value);
    }

    // int
    public void writeInt32(final int value) throws IOException {
        codedOutput.writeInt32NoTag(value);
    }

    // long
    public void writeInt64(final long value) throws IOException {
        codedOutput.writeInt64NoTag(value); // writeUInt64NoTag
    }

    // float
    public void writeFloat(final float value) throws IOException {
        codedOutput.writeFixed32NoTag(Float.floatToRawIntBits(value));
    }

    // double
    public void writeDouble(final double value) throws IOException {
        codedOutput.writeFixed64NoTag(Double.doubleToRawLongBits(value));
    }

    // boolean
    public void writeBoolean(final boolean value) throws IOException {
        codedOutput.writeBoolNoTag(value);
    }


    // --- Arrays ---

    public void write(final String[] values) throws IOException {

        // length of the array
        codedOutput.writeInt32NoTag(values.length);

        // data size in bits of the array
        int dataSize = 0;
        for (int i = 0; i < values.length; i++) {
            dataSize += CodedOutputStream.computeStringSizeNoTag(values[i]);
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the array
        for (int i = 0; i < values.length; i++) {
            codedOutput.writeStringNoTag(values[i]);
        }
    }

    public void write(final int[] values) throws IOException {

        // length of the array
        codedOutput.writeInt32NoTag(values.length);

        // data size in bits of the array
        int dataSize = 0;
        for (int i = 0; i < values.length; i++) {
            dataSize += CodedOutputStream.computeInt32SizeNoTag(values[i]);
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the array
        for (int i = 0; i < values.length; i++) {
            codedOutput.writeInt32NoTag(values[i]);
        }
    }


    public void write(final long[] values) throws IOException {

        // length of the array
        codedOutput.writeInt32NoTag(values.length);

        // data size in bits of the array
        int dataSize = 0;
        for (int i = 0; i < values.length; i++) {
            dataSize += CodedOutputStream.computeInt64SizeNoTag(values[i]);
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the array
        for (int i = 0; i < values.length; i++) {
            codedOutput.writeInt64NoTag(values[i]);
        }
    }

    public void write(final float[] values) throws IOException {

        // length of the array
        codedOutput.writeInt32NoTag(values.length);

        // data size in bits of the array
        int dataSize = 0;
        for (int i = 0; i < values.length; i++) {
            dataSize += CodedOutputStream.computeFixed32SizeNoTag(Float.floatToRawIntBits(values[i]));
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the array
        for (int i = 0; i < values.length; i++) {
            codedOutput.writeFixed32NoTag(Float.floatToRawIntBits(values[i]));
        }
    }

    public void write(final double[] values) throws IOException {

        // length of the array
        codedOutput.writeInt32NoTag(values.length);

        // data size in bits of the array
        int dataSize = 0;
        for (int i = 0; i < values.length; i++) {
            dataSize += CodedOutputStream.computeFixed64SizeNoTag(Double.doubleToRawLongBits(values[i]));
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the array
        for (int i = 0; i < values.length; i++) {
            codedOutput.writeFixed64NoTag(Double.doubleToRawLongBits(values[i]));
        }
    }

    public void write(final boolean[] values) throws IOException {

        // length of the array
        codedOutput.writeInt32NoTag(values.length);

        // data size in bits of the array
        int dataSize = 0;
        for (int i = 0; i < values.length; i++) {
            dataSize += CodedOutputStream.computeBoolSizeNoTag(values[i]);
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the array
        for (int i = 0; i < values.length; i++) {
            codedOutput.writeBoolNoTag(values[i]);
        }
    }

    // --- Maps ---

    public void writeMapInt2String(final HashMap<Integer, String> values) throws IOException {

        int size = values.size();

        // length of the map
        codedOutput.writeInt32NoTag(size);

        // data size in bits of the map
        int dataSize = 0;
        for (Map.Entry<Integer, String> entries : values.entrySet()) {
            dataSize += CodedOutputStream.computeInt32SizeNoTag(entries.getKey());
            dataSize += CodedOutputStream.computeStringSizeNoTag(entries.getValue());
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the map
        for (Map.Entry<Integer, String> entries : values.entrySet()) {
            codedOutput.writeInt32NoTag(entries.getKey());
            codedOutput.writeStringNoTag(entries.getValue());
        }


    }


    public void writeMapInt2Double(final HashMap<Integer, Double> values) throws IOException {

        int size = values.size();

        // length of the map
        codedOutput.writeInt32NoTag(size);

        // data size in bits of the map
        int dataSize = 0;
        for (Map.Entry<Integer, Double> entries : values.entrySet()) {
            dataSize += CodedOutputStream.computeInt32SizeNoTag(entries.getKey());
            dataSize += CodedOutputStream.computeFixed64SizeNoTag(Double.doubleToRawLongBits(entries.getValue()));
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the map
        for (Map.Entry<Integer, Double> entries : values.entrySet()) {
            codedOutput.writeInt32NoTag(entries.getKey());
            codedOutput.writeFixed64NoTag(Double.doubleToRawLongBits(entries.getValue()));
        }


    }

    public void writeMapString2String(final HashMap<String, String> values) throws IOException {

        int size = values.size();

        // length of the map
        codedOutput.writeInt32NoTag(size);

        // data size in bits of the map
        int dataSize = 0;
        for (Map.Entry<String, String> entries : values.entrySet()) {
            dataSize += CodedOutputStream.computeStringSizeNoTag(entries.getKey());
            dataSize += CodedOutputStream.computeStringSizeNoTag(entries.getValue());
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the map
        for (Map.Entry<String, String> entries : values.entrySet()) {
            codedOutput.writeStringNoTag(entries.getKey());
            codedOutput.writeStringNoTag(entries.getValue());
        }


    }

    public void writeMapString2Int(final HashMap<String, Integer> values) throws IOException {

        int size = values.size();

        // length of the map
        codedOutput.writeInt32NoTag(size);

        // data size in bits of the map
        int dataSize = 0;
        for (Map.Entry<String, Integer> entries : values.entrySet()) {
            dataSize += CodedOutputStream.computeStringSizeNoTag(entries.getKey());
            dataSize += CodedOutputStream.computeInt32SizeNoTag(entries.getValue());
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the map
        for (Map.Entry<String, Integer> entries : values.entrySet()) {
            codedOutput.writeStringNoTag(entries.getKey());
            codedOutput.writeInt32NoTag(entries.getValue());
        }


    }

    public void writeMapString2Double(final HashMap<String, Double> values) throws IOException {

        int size = values.size();

        // length of the map
        codedOutput.writeInt32NoTag(size);

        // data size in bits of the map
        int dataSize = 0;
        for (Map.Entry<String, Double> entries : values.entrySet()) {
            dataSize += CodedOutputStream.computeStringSizeNoTag(entries.getKey());
            dataSize += CodedOutputStream.computeFixed64SizeNoTag(Double.doubleToRawLongBits(entries.getValue()));
        }
        codedOutput.writeUInt32NoTag(dataSize);

        // data of the map
        for (Map.Entry<String, Double> entries : values.entrySet()) {
            codedOutput.writeStringNoTag(entries.getKey());
            codedOutput.writeFixed64NoTag(Double.doubleToRawLongBits(entries.getValue()));
        }


    }


}
