package fr.profi.mzdb.io.writer.mgf;

public class MgfPrecursor {

  private Double precMz = null;
  private Integer charge = null;
  private Float rt = null;

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

  public StringBuilder appendToStringBuilder(StringBuilder sb) {

    sb.append(MgfField.PEPMASS).append("=").append(String.format("%.4f", precMz)).append(MgfWriter.LINE_SPERATOR);
    if (charge != null) sb.append(MgfField.CHARGE).append("=").append(charge).append("+").append(MgfWriter.LINE_SPERATOR);
    if (rt != null) sb.append(MgfField.RTINSECONDS).append("=").append(String.format("%.2f", rt)).append(MgfWriter.LINE_SPERATOR);

    return sb;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    return this.appendToStringBuilder(sb).toString();
  }

}
