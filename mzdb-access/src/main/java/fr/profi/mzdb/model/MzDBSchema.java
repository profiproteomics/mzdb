package fr.profi.mzdb.model;

public class MzDBSchema {

  public static String getSchemaDDL() {
    StringBuilder sb = new StringBuilder();
    sb.append("CREATE TABLE data_processing (");
    sb.append("id INTEGER PRIMARY KEY AUTOINCREMENT,").append("name TEXT NOT NULL \n);\n");

    sb.append("CREATE TABLE scan_settings (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "param_tree,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id) \n);\n");

    sb.append( "CREATE TABLE data_encoding (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT, mode TEXT(10) NOT NULL,");
    sb.append( "compression TEXT, byte_order TEXT(13) NOT NULL,");
    sb.append( "mz_precision INTEGER NOT NULL,");
    sb.append( "intensity_precision INTEGER NOT NULL,");
    sb.append( "param_tree TEXT \n);\n");

    sb.append( "CREATE TABLE software (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "version TEXT NOT NULL,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id) \n);\n");

    sb.append( "CREATE TABLE processing_method (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "number INTEGER NOT NULL,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "data_processing_id INTEGER NOT NULL,");
    sb.append( "software_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id),");
    sb.append( "FOREIGN KEY (data_processing_id) REFERENCES data_processing (id),");
    sb.append( "FOREIGN KEY (software_id) REFERENCES software (id) \n);\n");

    sb.append( "CREATE TABLE sample (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "param_tree TEXT,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id) \n);\n");

    sb.append( "CREATE TABLE source_file (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "location TEXT NOT NULL,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id) \n);\n");

    sb.append( "CREATE TABLE source_file_scan_settings_map (");
    sb.append( "scan_settings_id INTEGER NOT NULL,");
    sb.append( "source_file_id INTEGER NOT NULL,");
    sb.append( "PRIMARY KEY (scan_settings_id, source_file_id) \n);\n");

    sb.append( "CREATE TABLE cv (");
    sb.append( "id TEXT(10) NOT NULL,");
    sb.append( "full_name TEXT NOT NULL,");
    sb.append( "version TEXT(10),");
    sb.append( "uri TEXT NOT NULL,");
    sb.append( "PRIMARY KEY (id) \n);\n");

    sb.append( "CREATE TABLE param_tree_schema (");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "type TEXT(10) NOT NULL,");
    sb.append( "schema TEXT NOT NULL,PRIMARY KEY (name) \n);\n");

    sb.append( "CREATE TABLE table_param_tree_schema (");
    sb.append( "table_name TEXT NOT NULL,");
    sb.append( "schema_name TEXT NOT NULL,");
    sb.append( "PRIMARY KEY (table_name),");
    sb.append( "FOREIGN KEY (schema_name) REFERENCES param_tree_schema (name) \n);\n");

    sb.append( "CREATE TABLE shared_param_tree (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "data TEXT NOT NULL,");
    sb.append( "schema_name TEXT NOT NULL,");
    sb.append( "FOREIGN KEY (schema_name) REFERENCES param_tree_schema (name) \n);\n");

    sb.append( "CREATE TABLE instrument_configuration (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "param_tree TEXT,");
    sb.append( "component_list TEXT NOT NULL,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "software_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id),");
    sb.append( "FOREIGN KEY (software_id) REFERENCES software (id) \n);\n");

    sb.append( "CREATE TABLE mzdb (");
    sb.append( "version TEXT(10) NOT NULL,");
    sb.append( "creation_timestamp TEXT NOT NULL,");
    sb.append( "file_content TEXT NOT NULL,");
    sb.append( "contacts TEXT NOT NULL,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "PRIMARY KEY (version) \n);\n");

    sb.append( "CREATE TABLE run (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "start_timestamp TEXT,");
    sb.append( "param_tree TEXT,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "sample_id INTEGER NOT NULL,");
    sb.append( "default_instrument_config_id INTEGER NOT NULL,");
    sb.append( "default_source_file_id INTEGER,");
    sb.append( "default_scan_processing_id INTEGER NOT NULL,");
    sb.append( "default_chrom_processing_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id),");
    sb.append( "FOREIGN KEY (sample_id) REFERENCES sample (id),");
    sb.append( "FOREIGN KEY (default_instrument_config_id) REFERENCES instrument_configuration (id),");
    sb.append( "FOREIGN KEY (default_source_file_id) REFERENCES source_file (id),");
    sb.append( "FOREIGN KEY (default_scan_processing_id) REFERENCES data_processing (id),");
    sb.append( "FOREIGN KEY (default_chrom_processing_id) REFERENCES data_processing (id) \n);\n");

    sb.append( "CREATE TABLE chromatogram (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "activation_type TEXT(10),"); // FIXME: was NOT NULL
    sb.append( "data_points BLOB NOT NULL,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "precursor TEXT,");
    sb.append( "product TEXT,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "run_id INTEGER NOT NULL,");
    sb.append( "data_processing_id INTEGER,");
    sb.append( "data_encoding_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id),");
    sb.append( "FOREIGN KEY (run_id) REFERENCES run (id),");
    sb.append( "FOREIGN KEY (data_processing_id) REFERENCES data_processing (id),");
    sb.append( "FOREIGN KEY (data_encoding_id) REFERENCES data_encoding (id) \n);\n");

    sb.append( "CREATE TABLE run_slice (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "ms_level INTEGER NOT NULL,");
    sb.append( "number INTEGER NOT NULL,");
    sb.append( "begin_mz REAL NOT NULL,");
    sb.append( "end_mz REAL NOT NULL,");
    sb.append( "param_tree TEXT,");
    sb.append( "run_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (run_id) REFERENCES run (id) \n);\n");

    sb.append( "CREATE TABLE spectrum (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "initial_id INTEGER NOT NULL,");
    sb.append( "title TEXT NOT NULL,");
    sb.append( "cycle INTEGER NOT NULL,");
    sb.append( "time REAL NOT NULL,");
    sb.append( "ms_level INTEGER NOT NULL,");
    sb.append( "activation_type TEXT(10),"); // FIXME: was NOT NULL
    sb.append( "tic REAL NOT NULL,");
    sb.append( "base_peak_mz REAL NOT NULL,");
    sb.append( "base_peak_intensity REAL NOT NULL,");
    sb.append( "main_precursor_mz REAL,");
    sb.append( "main_precursor_charge INTEGER,");
    sb.append( "data_points_count INTEGER NOT NULL,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "scan_list TEXT,");
    sb.append( "precursor_list TEXT,");
    sb.append( "product_list TEXT,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "instrument_configuration_id INTEGER,");
    sb.append( "source_file_id INTEGER,");
    sb.append( "run_id INTEGER NOT NULL,");
    sb.append( "data_processing_id INTEGER,");
    sb.append( "data_encoding_id INTEGER NOT NULL,");
    sb.append( "bb_first_spectrum_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id),");
    sb.append( "FOREIGN KEY (instrument_configuration_id) REFERENCES instrument_configuration (id),");
    sb.append( "FOREIGN KEY (source_file_id) REFERENCES source_file (id),");
    sb.append( "FOREIGN KEY (run_id) REFERENCES run (id),");
    sb.append( "FOREIGN KEY (data_processing_id) REFERENCES data_processing (id),");
    sb.append( "FOREIGN KEY (data_encoding_id) REFERENCES data_encoding (id),");
    sb.append( "FOREIGN KEY (bb_first_spectrum_id) REFERENCES spectrum (id) \n);\n");

    sb.append( "CREATE TABLE bounding_box (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "data BLOB NOT NULL,");
    sb.append( "run_slice_id INTEGER NOT NULL,");
    sb.append( "first_spectrum_id INTEGER NOT NULL,");
    sb.append( "last_spectrum_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (run_slice_id) REFERENCES run_slice (id),");
    sb.append( "FOREIGN KEY (first_spectrum_id) REFERENCES spectrum (id),");
    sb.append( "FOREIGN KEY (last_spectrum_id) REFERENCES spectrum (id) \n);\n");

    sb.append( "CREATE TABLE cv_term (");
    sb.append( "accession TEXT NOT NULL,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "unit_accession TEXT,");
    sb.append( "cv_id TEXT(10) NOT NULL,");
    sb.append( "PRIMARY KEY (accession),");
    sb.append( "FOREIGN KEY (unit_accession) REFERENCES cv_unit (accession),");
    sb.append( "FOREIGN KEY (cv_id) REFERENCES cv (id) \n);\n");

    sb.append( "CREATE TABLE cv_unit (");
    sb.append( "accession TEXT NOT NULL,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "cv_id TEXT(10) NOT NULL,");
    sb.append( "PRIMARY KEY (accession),");
    sb.append( "FOREIGN KEY (cv_id) REFERENCES cv (id) \n);\n");

    sb.append( "CREATE TABLE user_term (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "name TEXT NOT NULL,");
    sb.append( "type TEXT NOT NULL,");
    sb.append( "unit_accession TEXT,");
    sb.append( "FOREIGN KEY (unit_accession) REFERENCES cv_unit (accession) \n);\n");

    sb.append( "CREATE TABLE target (");
    sb.append( "id INTEGER PRIMARY KEY AUTOINCREMENT,");
    sb.append( "param_tree TEXT NOT NULL,");
    sb.append( "shared_param_tree_id INTEGER,");
    sb.append( "scan_settings_id INTEGER NOT NULL,");
    sb.append( "FOREIGN KEY (shared_param_tree_id) REFERENCES shared_param_tree (id),");
    sb.append( "FOREIGN KEY (scan_settings_id) REFERENCES scan_settings (id) \n);\n");

    sb.append( "CREATE VIRTUAL TABLE bounding_box_rtree USING rtree(");
    sb.append( "id INTEGER NOT NULL PRIMARY KEY,");
    sb.append( "min_mz REAL NOT NULL,");
    sb.append( "max_mz REAL NOT NULL,");
    sb.append( "min_time REAL NOT NULL,");
    sb.append( "max_time REAL NOT NULL \n);\n");

    sb.append( "CREATE VIRTUAL TABLE bounding_box_msn_rtree USING rtree(");
    sb.append( "id INTEGER NOT NULL PRIMARY KEY,");
    sb.append( "min_ms_level REAL NOT NULL,");
    sb.append( "max_ms_level REAL NOT NULL,");
    sb.append( "min_parent_mz REAL NOT NULL,");
    sb.append( "max_parent_mz REAL NOT NULL,");
    sb.append( "min_mz REAL NOT NULL,");
    sb.append( "max_mz REAL NOT NULL,");
    sb.append( "min_time REAL NOT NULL,");
    sb.append( "max_time REAL NOT NULL \n);\n");

    return sb.toString();
  }


}
