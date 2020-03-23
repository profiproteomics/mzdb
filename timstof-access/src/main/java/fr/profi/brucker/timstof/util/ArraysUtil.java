package fr.profi.brucker.timstof.util;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ArraysUtil {

    public static double[] copyFromIntArray(int[] source) {
        double[] dest = new double[source.length];
        for(int i=0; i<source.length; i++) {
            dest[i] = source[i];
        }
        return dest;
    }

    public static double[] copyFromIntList(List<Integer> source) {
        double[] dest = new double[source.size()];
        for(int i=0; i<source.size() ; i++) {
            dest[i] = source.get(i);
        }
        return dest;
    }

    public static double[]  doubleListToArray(List<Double> dblList){
        double[] result = new double[dblList.size()];
        for(int i =0 ; i<dblList.size(); i++)
            result[i] = dblList.get(i);
        return result;
    }

    public static float[]  floatListToArray(List<Float> floatlList){
        float[] result = new float[floatlList.size()];
        for(int i =0 ; i<floatlList.size(); i++)
            result[i] = floatlList.get(i);
        return result;
    }

    public static List<Integer> intArrayToIntegerList(int[] intarray){
        List<Integer> list = Arrays.stream(intarray).boxed().collect(Collectors.toList());
        return list;
    }


}
