# /usr/bin/env python3


from abc import ABCMeta
from db.SFNI4_species_codes import SFNI4_species
from models.trees.support_equations.equations_tree_models import TreeEquations

import sys
# import numpy as np
import pandas as pd
import os

# from simulator.src.data.variables import TREE_VARS
from data.variables import TREE_VARS


class SFNI4_volume(metaclass=ABCMeta):

    def __init__(self, configuration=None):
        super().__init__(name="SFNI4_volume", version=1)


    def catch_model_exception(self):  # that Function catch errors and show the line where they are
        exc_type, exc_obj, exc_tb = sys.exc_info()
        fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
        print('Oops! You made a mistake: ', exc_type, ' check inside ', fname, ' model, line', exc_tb.tb_lineno)


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
            df = SFNI4_volume.get_sfni_vol_parameters(tree, int(province), SFNI_version='SFNI4')
            if df is None or df.empty:
                # try to get the parameters from SFNI3
                df = SFNI4_volume.get_sfni_vol_parameters(tree, int(province), SFNI_version='SFNI3')

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


    def get_sfni_vol_parameters(tree, province, SFNI_version = 'SFNI4'):
        """
        Function that returns the volume parameters for the SFNI4 or SFNI3 model according to the species and location.
        Args:
            tree: Tree object
            province: Province code (INE)
            SFNI_version: SFNI version ('SFNI4' or 'SFNI3')
        """

        try:

            # access the desired csv file
            if SFNI_version == 'SFNI4':
                path = os.path.join(os.path.dirname(__file__), 'SFNI4_volume_coefficients', f'SFNI4_all_volume_coefficients_{province}.csv')
            elif SFNI_version == 'SFNI3':
                path = os.path.join(os.path.dirname(__file__), 'SFNI3_volume_coefficients', f'SFNI3_all_volume_coefficients_{province}.csv')
            else:
                path = None

            if not os.path.exists(path):
                # raise FileNotFoundError(f"The file {path} does not exist.")
                return None
            df = pd.read_csv(path)

            # get species name from code and tree shape
            if tree.species == 45:
                species_name = 'Quercus ilex'  # it has a sub-species name in the SFNI4 database
            else:
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


    def set_sfni4_vol_plot(plot, list_of_trees):
        """
        Function that sets the plot volume according to the SFNI4.
        Args:
            plot: Plot object
            list_of_trees: List that contains all the trees in the plot
        """

        try:

            # initialize variables
            plot_vcc = plot_vsc = plot_vle = plot_iavc = 0

            # define attributes and plot attributes
            attributes = ['sfni_vcc', 'sfni_vsc', 'sfni_vle', 'sfni_iavc']

            plot_attributes = [plot_vcc, plot_vsc, plot_vle, plot_iavc]

            # for each tree, we are going to add the individual values to the plot value
            for tree in list_of_trees:

                # iterate over the list of attributes and plot attributes
                for attr, plot_attr, n in zip(attributes, plot_attributes, range(len(plot_attributes))):
                    if attr in TREE_VARS:  # if the attribute is in the list of variables, add it to the plot object
                        value = getattr(tree, attr, '')
                        if value != '':  # if the value is not empty, add it to the plot attribute
                            plot_attr += value * tree.expan / 1000
                            plot_attributes[n] = plot_attr  # update the plot attribute
                        else:
                            plot_attributes[n] = plot_attr  # previous value is maintained

            # define features and values as tuples
            features_and_values = [
                ('SFNI_VCC', plot_attributes[0]),
                ('SFNI_VSC', plot_attributes[1]),
                ('SFNI_VLE', plot_attributes[2]),
                ('SFNI_IAVC', plot_attributes[3])
            ]

            # set the values to the plot object
            TreeEquations.set_value_to_feature_plot(plot, features_and_values)

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