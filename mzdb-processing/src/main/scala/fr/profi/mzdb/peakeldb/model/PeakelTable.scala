package fr.profi.mzdb.peakeldb.model

object PeakelTable {

  val ID = "id"
  val MOZ = "moz"
  val RT = "elution_time"
  val DURATION = "duration"
  val GAP_COUNT = "gap_count"
  val APEX_INTENSITY = "apex_intensity"
  val area = "area"
  val LEFT_HWHM_MEAN = "left_hwhm_mean"
  val RIGHT_HWHM_MEAN = "right_hwhm_mean"
  val PEAK_COUNT = "peak_count"
  val PEAKS = "peaks"
  val FIRST_SPECTRUM_ID = "first_spectrum_id"
  val APEX_SPECTRUM_ID = "apex_spectrum_id"
  val LAST_SPECTRUM_ID = "last_spectrum_id"
  val MAP_ID ="map_id"

  def tableName: String ="peakel"

}


