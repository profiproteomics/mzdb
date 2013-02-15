package fr.profi.mzdb.algo.ms.calibration

import fr.profi.mzdb.model.Peak

class ClassedPeakScala(mz: Double, intensity: Float, val order: Int) extends Peak(mz,intensity)