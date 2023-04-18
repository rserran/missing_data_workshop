# Pima Indians Diabetes Dataset - EDA/Iterative Imputer
# Source: https://www.kaggle.com/gifarihoque/pidd-missing-data-ml-iterimputer-tut-86

# import libaries
import pandas as pd
import numpy as np

from pandas_profiling import profile_report, ProfileReport

from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.ensemble import ExtraTreesRegressor

# read dataset from source (UCI)
data_web_address = "https://raw.githubusercontent.com/LamaHamadeh/Pima-Indians-Diabetes-DataSet-UCI/master/pima_indians_diabetes.txt"

column_names = ['pregnancies', 
                'glucose', 
                'blood_pressure', 
                'skin_thickness', 
                'insulin', 
                'bmi', 
                'diabetes_pedigree_func', 
                'age', 
                'target']

feature_names = column_names[:-1]
df = pd.read_csv(data_web_address , names=column_names)
df.head()

df.info()

# statitiscal summary for numerical features
df.describe().T

# plot histograms
df.hist(figsize = (10, 10))

# let's examine the zeros in each feature
df_copy = df.copy()

(df == 0).sum()

# features that zeros represent NaNs
# let's substitute those zeros to NaNs for selected columns
cols_selected = ['glucose', 'blood_pressure', 'skin_thickness', 'insulin', 'bmi']

df[cols_selected] = df[cols_selected].replace(0, np.NaN)

# let's verify missing values
df.isnull().sum()

df.hist(figsize = (10, 10))

# pandas profiling
profile = df.profile_report(title = 'PIMA Indians Diabetes Dataset Profile')

profile.to_file('pima_diabetes.html')

# impute missing values
imp = IterativeImputer(estimator = ExtraTreesRegressor(), max_iter = 100, random_state = 42)

imp.fit(df)

df_imp = pd.DataFrame(imp.transform(df), columns = df.columns)

df_imp.isnull().sum()

# values less than zero
df_imp.le(0).any()

df_imp.describe().T

df_imp.hist(figsize = (10, 10))

# output to csv
df_imp.to_csv('pima_diabetes.imp.csv', index = False)