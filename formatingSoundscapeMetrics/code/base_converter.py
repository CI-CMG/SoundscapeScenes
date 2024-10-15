import os
import json
import shutil
import logging

from configparser import ConfigParser

import common_tools.utilities as ut

log = logging.getLogger('netCDF')


class BaseConverter(object):
    """Base class for csv file to netCDF PAD converters. This base class
    handles the request and parsing of a settings file and system.ini
    file.

    """
    def __init__(self, get_settings=True):

        # Instantiate attributes.
        self.package_path = None
        self.metadata = None
        self.manifest = None

        # Get settings file to use and grab DOI URl from file (may be Null is
        # we'll be minting DOIs per dataset.
        if get_settings:
            settings_file = self.get_settings_path()
            with open(settings_file) as f:
                settings = json.load(f)

            self.citation = settings['citationText']
            self.doi = settings['doi']
            self.doi_minting = settings['mintDOI']
            self.version = settings['version']

            if 'acknowledgement' in settings:
                self.acknowledgement = settings['acknowledgement']

        # Get root directory of CPI system.
        cwd = os.getcwd()
        cpi_root = os.path.join(cwd.split('/CPI')[0], 'CPI')

        # Read system.ini file
        system_file = os.path.join(cpi_root, 'system.ini')
        system = ConfigParser()
        system.read(system_file)
        self.starting_path = system.get('Default', 'startingPath')
        self.log_base = system.get('Default', 'logBasePath')
        self.log_level = system.get('Default', 'logLevel')

    def get_settings_path(self):
        """Get path to settings file

        The settings file is a json file containing control parameters.
        For netCDF conversion this is used to pass in existing DOI
        information only despite the settings file (also used for
        pre-ingest)  contains many more values.

        This method asks the user for the name of the settings file to use.
        Settings files are located in a folder named cpi_settings/pad in the
        user's home directory. The important key/value pairs for this method is
        "doi": "https://doi.org/10.25921/saca-sp25" "citation": "Citation text".

        Returns:
            settings_path(str): path to the settings json file.

        """
        # Get list of settings files in user home directory
        root_path = os.path.join(os.path.expanduser("~"),
                                 'cpi_settings/pad')
        root, dirs, files = ut.walk_dir(root_path)
        files.sort()
        print(f'\nSettings files in {root}:  {files}')
        settings_file = input("Enter name of settings file to use: ")
        if not os.path.splitext(settings_file)[1]:
            settings_file = settings_file + '.json'
        settings_path = os.path.join(root, settings_file)
        # settings_path = os.path.join(root, 'nmfs.json')
        if os.path.isfile(settings_path):
            return settings_path
        else:
            print(f'{settings_path} is not a valid file path. '
                  f'Enter name of the file again: ')
            return self.get_settings_path()

    def get_metadata(self, metadata_file):
        """Open metadata file and convert to Python dictionary held in
        self.metadata attribute.

        Args:
            metadata_file (str): Full path to metadata file

        """
        with open(metadata_file, 'r') as f:
            self.metadata = json.loads(f.read())

        self.metadata['DOI'] = self.doi

    def parse_manifest(self, manifest_path, backup=True):
        """Parse manifest file and convert into a dictionary with file path and
        key and checksum as value and store in attribute self.manifest.

        Args:
            manifest_path (str): Full path to manifest file.
            backup (boo): Control whether to make copy of manifest file before
                          parsing.

        """
        if backup:
            # Make a backup of the original manifest before continuing. If a
            # backup already exists skip overwriting to preserve original
            # manifest when doing multiple processing runs.
            path_parts = os.path.split(manifest_path)
            backup_path = os.path.join(path_parts[0],
                                       f'original-{path_parts[1]}')
            if not os.path.isfile(backup_path):
                shutil.copy2(manifest_path, backup_path)

        manifest = {}
        with open(manifest_path) as m:
            for line in m:
                entry = line.split('  ')
                # print(entry[1][:-1], entry[0])
                manifest[entry[1][:-1]] = entry[0]

        self.manifest = manifest

    def mint_doi(self):
        """Mint DOI. This is a placeholder for eventual development.

        """
        print(f'Code can not yet mint a dataset;level DOI')
        log.critical(f'Code can not yet mint a dataset-level DOI')
        self.metadata['DOI'] = ''



