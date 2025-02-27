package fr.profi.mzdb.io.writer.mgf;

import java.util.HashMap;
import java.util.Map;

public class MgfPrecursor {

  private Double precMz = null;
  private Integer charge = null;
  private Float rt = null;

  private Map<String, Object> annotations = null;

  /**
   *
   * @param precMz
   * @param charge
   * @return a new MgfHeader
   */
  public MgfPrecursor(double precMz, int charge) {
    this(Double.valueOf(precMz), Integer.valueOf(charge), null);
  }

  /**
   *
   * @param precMz
   * @param rt
   * @return a new MgfHeader
   */
  public MgfPrecursor(double precMz, float rt) {
    this(Double.valueOf(precMz), null, Float.valueOf(rt));
  }

  /**
   *
   * @param precMz
   * @param charge
   * @param rt
   * @return a new {@link MgfPrecursor}
   */
  public MgfPrecursor(double precMz, int charge, float rt) {
    this(Double.valueOf(precMz), Integer.valueOf(charge), Float.valueOf(rt));
  }

  public MgfPrecursor(Double precMz, Integer charge, Float rt) {
    this.precMz = precMz;
    this.charge = charge;
    this.rt = rt;
  }

  public Double getPrecMz() {
    return precMz;
  }

  public Integer getCharge() {
    return charge;
  }

  public Float getRt() {
    return rt;
  }

  private Map<String, Object> getAnnotations() {
    if (annotations == null) {
      annotations = new HashMap<>();
    }
    return annotations;
  }

  public void addAnnotation(String key, Object value) {
    getAnnotations().put(key, value);
  }

  public Object getAnnotationOrElse(String key, Object defaultValue) {
    return (annotations == null) ? defaultValue : annotations.getOrDefault(key, defaultValue);
  }

  public boolean hasAnnotation(String key) {
    return (annotations != null) && annotations.containsKey(key);
  }

  public Object getAnnotation(String key) {
    return (annotations == null) ? null : annotations.get(key);
  }


  public StringBuilder appendToStringBuilder(StringBuilder sb) {

    sb.append(MgfField.PEPMASS).append("=").append(String.format("%.4f", precMz)).append(MgfWriter.LINE_SPERATOR);
    if (charge != null) sb.append(MgfField.CHARGE).append("=").append(charge).append("+").append(MgfWriter.LINE_SPERATOR);
    if (rt != null) sb.append(MgfField.RTINSECONDS).append("=").append(String.format("%.2f", rt)).append(MgfWriter.LINE_SPERATOR);

    return sb;
  }

  public MgfPrecursor clone(double precMz, int charge, float rt) {
    MgfPrecursor newPrecursor = new MgfPrecursor(precMz, charge, rt);
    if (annotations != null) {
      for (Map.Entry<String, Object> entry : annotations.entrySet()) {
        newPrecursor.addAnnotation(entry.getKey(), entry.getValue());
      }
    }
    return newPrecursor;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    return this.appendToStringBuilder(sb).toString();
  }

}
