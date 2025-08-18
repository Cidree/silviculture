# /usr/bin/env python3



import sys
# import numpy as np
import pandas as pd
import os



class SFNI4_volume(metaclass=ABCMeta):

    def __init__(self, configuration=None):
        super().__init__(name="SFNI4_volume", version=1)


    def set_sfni4_vol_tree(tree, mean_plot_dbh, province):
        """
        Function that sets the volume values for each species according to the SFNI4.
        Parameters for each species and location are defined in the function sfni4_vol_parameters.
        Args:
            tree: Tree object
            mean_plot_dbh: Mean plot dbh (cm)
            province: Province code (INE)
        Source:
            Doc.: SFNI4 documentation:
            https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf
        """

        try:

            # if the province is not defined, return None
            if province in [None, ''] or isinstance(province, str):
                SFNI4_volume.empty_results(tree)
                return

            # get parameters for each species and location
            df = SFNI4_volume.get_sfni4_vol_parameters(tree, int(province))
            if df is None or df.empty:
                SFNI4_volume.empty_results(tree)
                return

            # variables
            vcc = vsc = vle = iavc = ''
            dbh_mm = tree.dbh * 10  # dbh in mm
            dnm_mm = mean_plot_dbh * 10  # mean plot dbh in mm
            h_m = tree.height  # height in m

            # VCC calculations
            df_vcc = df[df['Parametro'] == 'VCC']
            model_vcc = df_vcc['Modelo'].values[0]
            if h_m in ['', 0]:
                vcc = ''
            else:
                if model_vcc == 1:
                    if all(df_vcc[param].values[0] not in ['', '-'] for param in ['a', 'b']):
                        vcc = float(df_vcc['a'].values[0]) + float(df_vcc['b'].values[0]) * (dbh_mm) ** 2 * h_m
                elif model_vcc == 11:
                    if all(df_vcc[param].values[0] not in ['', '-'] for param in ['p', 'q', 'r']):
                        vcc = float(df_vcc['p'].values[0]) * (dbh_mm) ** float(df_vcc['q'].values[0]) * h_m ** float(df_vcc['r'].values[0])

            # VSC calculations
            df_vsc = df[df['Parametro'] == 'VSC']
            model_vsc = df_vsc['Modelo'].values[0]
            if vcc in ['', 0]:
                vsc = ''
            else:
                if model_vsc == 7:
                    if all(df_vsc[param].values[0] not in ['', '-'] for param in ['a', 'b', 'c']):
                        vsc = float(df_vsc['a'].values[0]) + float(df_vsc['b'].values[0]) * vcc + float(df_vsc['c'].values[0]) * vcc ** 2

            # VLE calculations
            df_vle = df[df['Parametro'] == 'VLE']
            model_vle = df_vle['Modelo'].values[0]
            if model_vle == 10:
                if h_m in ['', 0]:
                    vle = ''
                else:
                    if all(df_vle[param].values[0] not in ['', '-'] for param in ['a', 'b', 'c']):
                        vle = float(df_vle['a'].values[0]) + float(df_vle['b'].values[0]) * vcc + float(df_vle['c'].values[0]) * vcc ** 2
            elif model_vle == 12:
                if all(df_vle[param].values[0] not in ['', '-'] for param in ['p', 'q']):
                    vle = float(df_vle['p'].values[0]) * (dbh_mm) ** float(df_vle['q'].values[0])

            # IAVC calculations
            df_iavc = df[df['Parametro'] == 'IAVC']
            model_iavc = df_iavc['Modelo'].values[0]
            if model_iavc == 8:
                if vcc in ['', 0]:
                    iavc = ''
                else:
                    if all(df_iavc[param].values[0] not in ['', '-'] for param in ['a', 'b', 'c']):
                        iavc = float(df_iavc['a'].values[0]) + float(df_iavc['b'].values[0]) * vcc + float(df_iavc['c'].values[0]) * vcc ** 2
            elif model_iavc == 13:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['a', 'b']):
                    iavc = float(df_iavc['a'].values[0]) + float(df_iavc['b'].values[0]) * (dbh_mm - dnm_mm)
            elif model_iavc == 14:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['p', 'q']):
                    iavc = float(df_iavc['p'].values[0]) * (dbh_mm) ** float(df_iavc['q'].values[0])
            elif model_iavc == 16:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['a', 'b']):
                    iavc = float(df_iavc['a'].values[0]) + float(df_iavc['b'].values[0]) * (dbh_mm) ** 2
            elif model_iavc == 17:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['a', 'b', 'c']):
                    iavc = float(df_iavc['a'].values[0]) + float(df_iavc['b'].values[0]) * (dbh_mm) ** 2 + float(df_iavc['c'].values[0]) * (dbh_mm) ** 2
            elif model_iavc == 19:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['a', 'b', 'c', 'd']):
                    iavc = float(df_iavc['a'].values[0]) + float(df_iavc['b'].values[0]) * (dbh_mm) ** 2 + float(df_iavc['c'].values[0]) * (dbh_mm) ** 2 + float(df_iavc['d'].values[0]) * (dbh_mm) ** 3
            elif model_iavc == 20:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['a', 'b', 'd']):
                    iavc = float(df_iavc['a'].values[0]) + float(df_iavc['b'].values[0]) * (dbh_mm) ** 2 + float(df_iavc['d'].values[0]) * (dbh_mm) ** 3
            elif model_iavc == 21:
                if all(df_iavc[param].values[0] not in ['', '-'] for param in ['c', 'd']):
                    iavc = float(df_iavc['c'].values[0]) * (dbh_mm) ** 2 + float(df_iavc['d'].values[0]) * (dbh_mm) ** 3
            elif model_iavc == 25:
                if h_m in ['', 0]:
                    iavc = ''
                else:
                    if all(df_iavc[param].values[0] not in ['', '-'] for param in ['p', 'q', 'r']):
                        iavc = float(df_iavc['p'].values[0]) * (dbh_mm) ** float(df_iavc['q'].values[0]) * (h_m) ** float(df_iavc['r'].values[0])

            # assign volume to tree
            tree.add_value('sfni_vcc', vcc)
            tree.add_value('sfni_vsc', vsc)
            tree.add_value('sfni_vle', vle)
            tree.add_value('sfni_iavc', iavc)

        except Exception:
            SFNI4_volume.catch_model_exception()


    def get_sfni4_vol_parameters(tree, province):
        """
        Function that returns the volume parameters for the SFNI4 model according to the species and location.
        Args:
            tree: Tree object
            province: Province code (INE)
        """

        try:

            # access the desired csv file
            path = os.path.join(os.path.dirname(__file__), 'SFNI4_volume_coefficients', f'SFNI4_all_volume_coefficients_{province}.csv')
            if not os.path.exists(path):
                # raise FileNotFoundError(f"The file {path} does not exist.")
                return None
            df = pd.read_csv(path)

            # get species name from code and tree shape
            if tree.species == 45:
                species_name = 'Quercus ilex'  # it has a sub-species name in the SFNI4 database
            else:
                #species_code = SFNI4_species.get_sfni4_sp_code('Pinus nigra')
                species_name = SFNI4_species.get_sfni4_sp_name(tree.species)

            if 'shape' in TREE_VARS and tree.shape not in ['', 0]:
                shp = tree.shape
            else:
                shp = 'default'  # by default value

            # extract coefficients
            df = df[df['Especie'] == species_name]

            # when no shape value, all the possibilities are tested
            if shp == 'default':
                shp_values = [1, 2, 3, 4, 5, 6]
                for val in shp_values:
                    df_filtered = df[df['F.c.'] == val]
                    if not df_filtered.empty:
                        shp = val
                        df = df_filtered
                        break  # out of the loop when a value is found
            df = df[df['F.c.'] == shp]

            return df

        except Exception:
            SFNI4_volume.catch_model_exception()


    def empty_results(tree):
        """
        Function that sets the volume values to empty when it is not possible to calculate them.
        Args:
            tree: Tree object
        """

        tree.add_value('sfni_vcc', '')
        tree.add_value('sfni_vsc', '')
        tree.add_value('sfni_vle', '')
        tree.add_value('sfni_iavc', '')
