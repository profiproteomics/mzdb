package fr.profi.mzdb.serialization;

import com.google.protobuf.CodedInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;



public class SerializationReader {

    private CodedInputStream codedInput;

    public SerializationReader(InputStream inputStream) {
        codedInput = CodedInputStream.newInstance(inputStream);
    }


    // --- Base Types ---

    public String readString() throws IOException {
        String value = this.codedInput.readStringRequireUtf8(); //JPM.OM.FIX
         return value;
    }

    public int readInt32() throws IOException {
        return codedInput.readInt32();
    }

    // long
    public long readInt64() throws IOException {
        return codedInput.readInt64();  // readInt64
    }

    public float readFloat() throws IOException {
        return codedInput.readFloat();
    }

    public double readDouble() throws IOException {
        return codedInput.readDouble();
    }

    public boolean readBoolean() throws IOException {
        return codedInput.readBool();
    }

    // --- Arrays ---

    public String[] readArrayString() throws IOException {

        int size = codedInput.readInt32();
        String[] array = new String[size];

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);
        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            array[i] = codedInput.readStringRequireUtf8();
            i++;
        }
        codedInput.popLimit(limit);

        return array;
    }

    public int[] readArrayInt32() throws IOException {

        int size = codedInput.readInt32();
        int[] array = new int[size];

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);
        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            array[i] = codedInput.readInt32();
            i++;
        }
        codedInput.popLimit(limit);

        return array;
    }

    public long[] readArrayInt64() throws IOException {

        int size = codedInput.readInt32();
        long[] array = new long[size];

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);
        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            array[i] = codedInput.readInt64();
            i++;
        }
        codedInput.popLimit(limit);

        return array;
    }

    public float[] readArrayFloat() throws IOException {

        int size = codedInput.readInt32();
        float[] array = new float[size];

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);
        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            array[i] = codedInput.readFloat();
            i++;
        }
        codedInput.popLimit(limit);

        return array;
    }

    public double[] readArrayDouble() throws IOException {

        int size = codedInput.readInt32();
        double[] array = new double[size];

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);
        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            array[i] = codedInput.readDouble();
            i++;
        }
        codedInput.popLimit(limit);

        return array;
    }

    public boolean[] readArrayBoolean() throws IOException {

        int size = codedInput.readInt32();
        boolean[] array = new boolean[size];

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);
        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            array[i] = codedInput.readBool();
            i++;
        }
        codedInput.popLimit(limit);

        return array;
    }


    // --- Maps ---

    public HashMap<Integer, String> readMapIntString() throws IOException {

        int size = codedInput.readInt32();
        HashMap<Integer, String> map = new HashMap<>(size);

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);

        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            int key = codedInput.readInt32();
            String value = codedInput.readStringRequireUtf8();
            map.put(key, value);
            i++;
        }
        codedInput.popLimit(limit);

        return map;
    }

    public HashMap<Integer, Double> readMapIntDouble() throws IOException {

        int size = codedInput.readInt32();
        HashMap<Integer, Double> map = new HashMap<>(size);

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);

        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            int key = codedInput.readInt32();
            Double value = codedInput.readDouble();
            map.put(key, value);
            i++;
        }
        codedInput.popLimit(limit);

        return map;
    }

    public HashMap<String, String> readMapStringString() throws IOException {

        int size = codedInput.readInt32();
        HashMap<String, String> map = new HashMap<>(size);

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);

        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            String key = codedInput.readStringRequireUtf8();
            String value = codedInput.readStringRequireUtf8();
            map.put(key, value);
            i++;
        }
        codedInput.popLimit(limit);

        return map;
    }

    public HashMap<String, Integer> readMapStringInteger() throws IOException {

        int size = codedInput.readInt32();
        HashMap<String, Integer> map = new HashMap<>(size);

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);

        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            String key = codedInput.readStringRequireUtf8();
            Integer value = codedInput.readInt32();
            map.put(key, value);
            i++;
        }
        codedInput.popLimit(limit);

        return map;
    }

    public HashMap<String, Double> readMapStringDouble() throws IOException {

        int size = codedInput.readInt32();
        HashMap<String, Double> map = new HashMap<>(size);

        int length = codedInput.readRawVarint32();
        int limit = codedInput.pushLimit(length);

        int i = 0;
        while (codedInput.getBytesUntilLimit() > 0) {
            String key = codedInput.readStringRequireUtf8();
            Double value = codedInput.readDouble();
            map.put(key, value);
            i++;
        }
        codedInput.popLimit(limit);

        return map;
    }
}
