package fr.profi.mzdb.model

case class OverlappingFeature(
  
  // feature with ambiguous monositopic peak
  feature: Feature,

  // index of the peakel being polluted in the overlapped feature
  overlappedPeakelIndex: Int,
  
  // index of the peakel of the overlapping feature polluting the overlapped feature
  overlappingPeakelIndex: Int,
  
  // distance in cycle between overlapped peak and and overlapping peak
  apexDistanceInNbCycle: Int,
  
  // correlation between the peakel 'shape'
  pCorr: Float,
  
  // quotient to check against the averagine
  avgQuot: Float
  
)