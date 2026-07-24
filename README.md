# mzdb

This repository contains few librairies around mzDB files format

## mzDB Access
Java library for reading mzDB files and creating mzDB model objects (bounding box/run slices etc...)

## mzDB processing
Java library for runing process on mzDB files particualry peakel extraction.

## timstof access
Java library to read brucker Timstof files and converting them to mzDB format.

# Releases Notes

## 1.5.0 (SNAPSHOT) 

Changes from version 1.3.1


**Main Modifications and Improvements**

****1. FeatureDb Support (New Feature)****
- Introduced `FeatureDbReader` and `FeatureDbWriter` to allow reading and storing quantification Features directly into a SQLite-based Feature database (`featureDb`).
- Updated `MzDbFeatureDetector` to support this new storage mechanism.
- Refactored `PeakelDbHelper` and `PeakelDbReader` to integrate with the Feature storage logic.

****2. MGFBoost Algorithm Enhancements (v3.6.5)****
- Improved precursor extraction logic: the algorithm now extracts a complete list of precursors from scan selectors, summarizes them, and applies filtering based on Precursor Intensity Fraction (PIF) and rank.
- Simplified MGFBoost parameters for better usability.
- Renamed and refactored `IsolationWindowPrecursorExtractor` to `MgfBoostPrecursorExtractor`.
- Added support for alternative precursor filtering and PIF calculation.

****3. Performance Optimizations****
- **Dot Product Calculation:** Optimized the performance of dot product calculations in `IsotopicPatternScorer`.
- **String Formatting:** Improved the efficiency of string formatting and spectrum stringification in `MgfWriter`.
- **Data Source Efficiency:** Enhanced `buildDataSource` efficiency in `MgfBoostPrecursorExtractor`.

****4. Core Logic and API Updates****
- **Ion Mobility:** Replaced "Compensation voltage values" with "Separation values" to generalize support for various ion mobility types beyond FAIMS.
- **Spectrum Management:** Added the ability to change `SpectrumHeader` titles, facilitating the splitting and reindexing of spectra.
- **MzdbClient:** Added a method to read acquisition metadata via socket.
- **Algorithm Refinement:** Improved RunSlice ID creation to specifically handle MS2 and MS3 levels, preventing ID overlaps.

****5. Bug Fixes and Refactoring****
- Fixed potential overlapping IDs between Max MS1 and the first MS2/MS3 scans.
- Improved error reporting by printing full file paths when a `FileNotFoundException` occurs.
- Conducted general code cleanup and removed obsolete log traces from the repository.
- Updated various dependency versions to address vulnerabilities and ensure compatibility with `profi-pom`.

