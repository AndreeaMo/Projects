import matplotlib.pyplot as plt
import seaborn as sb
import pandas as pd
from sklearn.model_selection import train_test_split
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.feature_selection import RFE
from sklearn.linear_model import LinearRegression
from statsmodels.stats.outliers_influence import variance_inflation_factor
import statsmodels.api as sm
from termcolor import colored as cl
from scipy import stats

# Citire date
tabel_zile = pd.DataFrame(pd.read_excel("Tabel_date_initiale_.xlsx"))

nume_variabile = list(tabel_zile.columns)
print(nume_variabile)
variabile_numerice = nume_variabile[2:]
x = tabel_zile[variabile_numerice].values

# Inlocuire valori null

def nan_replace(tabel):
    assert isinstance(tabel, pd.DataFrame)
    nume_variabile = list(tabel.columns)
    for variabila in nume_variabile:
        if any(tabel[variabila].isna()):
            if tip.is_numeric_dtype(tabel[variabila]):
                tabel[variabila].fillna(tabel[variabila].mean(), inplace=True)
            else:
                modul = tabel[variabila].mode()[0]
                tabel[variabila].fillna(modul, inplace=True)


nan_replace(tabel_zile)

missing_val = tabel_zile.isna().sum()
print(missing_val)

# Verificare valori duplicate
tabel_zile.duplicated(subset=None, keep='first')

# curatare tabel - eliminare coloane nefolositoare
print(tabel_zile.describe())
columns_to_drop = ['instant', 'dteday', 'casual', 'registered']
tabel_zile.drop(columns_to_drop, inplace=True, axis=1)
print(tabel_zile)
tabel_zile.index.name = "Index"
tabel_zile.to_csv('Tabel_nou.csv')
tabel_zile.info()

# Analiza exploratorie

# Corelograma
plt.figure(figsize=(40, 35))
corr = sb.heatmap(tabel_zile.corr(), annot=True, cmap="coolwarm", fmt='.2f')
plt.yticks(rotation=0)
plt.show()

# Bar chart
sb.set_style('darkgrid')
sb.barplot(x='season', y='cnt',
           data=tabel_zile.replace({'season': {1: 'Primavara', 2: 'Vara', 3: 'Toamna', 4: 'Iarna'}},
                                   inplace=False), ci=None, alpha=0.8)
sb.color_palette('pastel')
plt.xlabel('Anotimpuri')
plt.ylabel('Nr. inchirieri')
plt.title('Comparatie intre anotimpuri', fontdict={'family': 'serif', 'size': 16})
plt.show()
print("Media inchirierilor :", tabel_zile[tabel_zile['season'] == 3]['cnt'].mean())

# Pie chart
zi_lucratoare = tabel_zile[tabel_zile['workingday'] == 1]['cnt'].count()
zi_libera = tabel_zile[tabel_zile['workingday'] == 0]['cnt'].count()

plt.pie([zi_libera, zi_lucratoare], labels=['Nr inchirieri - zi lucratoare', 'Nr inchirieri-zi libera'],
        explode=[0.1, 0],
        startangle=10, shadow=True)
plt.show()

# Sumarizare date
media_pe_luna = pd.DataFrame()
medii = []
media_pe_luna['mnth'] = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September',
                         'October', 'November', 'December']
for i in range(1, 13):
    medii.append(tabel_zile[tabel_zile['mnth'] == i]['cnt'].mean())
media_pe_luna['mean'] = medii
media_pe_luna.sort_values('mean', ascending=False, inplace=True)
print("Numarul mediu de inchirieri pe luna in ordine descrescatoare :", round(media_pe_luna, 2), sep='\n')
media_pe_luna.index.name = "Index"
media_pe_luna.to_csv("Tabel_sumarizare_medii.csv")

print(tabel_zile.kurt(numeric_only=True))

# Printare info pentru variabilele numerice pentru identificare outlieri:
print(tabel_zile['atemp'].describe())
print(tabel_zile['windspeed'].describe())
print(tabel_zile['hum'].describe())
print(tabel_zile['temp'].describe())


# Detectat outlieri si eliminare
def outlieri(tabel_zile, nume_col):
    for x in [nume_col]:
        Q75, Q25 = np.percentile(tabel_zile.loc[:, x], [75, 25])
        intr_qr = Q75 - Q25

        max = Q75 + (1.5 * intr_qr)
        min = Q25 - (1.5 * intr_qr)

        tabel_zile.loc[tabel_zile[x] < min, x] = np.nan
        tabel_zile.loc[tabel_zile[x] > max, x] = np.nan
    return tabel_zile.dropna(axis=0)


for x in ['atemp', 'hum', 'temp', 'windspeed']:
    tabel_zile = outlieri(tabel_zile, x)

print(tabel_zile.shape)

t_curatat = tabel_zile
t_curatat.to_csv("Date Curatate.csv")

# standardizare
scaler = MinMaxScaler()
num_vars = ['temp', 'atemp', 'hum', 'windspeed', 'cnt']

t_curatat[num_vars] = scaler.fit_transform(t_curatat[num_vars])

# Create dummy variables
t_curatat['season'] = t_curatat['season'].astype('category')
t_curatat['weathersit'] = t_curatat['weathersit'].astype('category')
t_curatat['mnth'] = t_curatat['mnth'].astype('category')
t_curatat['weekday'] = t_curatat['weekday'].astype('category')

t_curatat.info()

tabel_1 = pd.get_dummies(t_curatat, drop_first=True)
print(tabel_1)
tabel_1.to_csv("Tabel cu variabile dummy.csv")
tabel_1.shape

# Construire model
y = tabel_1[['cnt']]
x = tabel_1.loc[:, tabel_1.columns != 'cnt']

# Imparitrea datelor
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=100)

from sklearn.feature_selection import RFE

# tehnica de eliminare a caracteristicilor recursive
regression = LinearRegression()
regression.fit(x_train, y_train)

rfe = RFE(regression, 15)
rfe = rfe.fit(x_train, y_train)
print(list(zip(x.columns, rfe.support_, rfe.ranking_)), sep="\n")

col = x.columns[rfe.support_]
x_rfe = x[col]

x_train_rfe = x_train[col]

# VIF - factorul de inflatie a variantei
vif = pd.DataFrame()
vif['Variabile'] = x_rfe.columns
vif['VIF'] = [variance_inflation_factor(x_rfe.values, i) for i in range(x_rfe.shape[1])]
vif['VIF'] = round(vif['VIF'], 2)
vif = vif.sort_values(by="VIF", ascending=False)
print(vif)

# Model1
x_train_1 = sm.add_constant(x_train_rfe)
regression1 = sm.OLS(y_train, x_train_1).fit()
print(regression1.summary())

# elminiare variabile pe baza p-value si a factorului de inflatie a variantei
x_new = x_train_rfe.drop(['holiday', 'weekday_1', 'weekday_2', 'weekday_3', 'workingday', 'weekday_5', 'weekday_4'],
                         axis=1)

# VIF 2
vif = pd.DataFrame()
vif['Variabile'] = x_new.columns
vif['VIF'] = [variance_inflation_factor(x_new.values, i) for i in range(x_new.shape[1])]
vif['VIF'] = round(vif['VIF'], 2)
vif = vif.sort_values(by="VIF", ascending=False)
print(vif)

# Model 2
x_train_2 = sm.add_constant(x_new)
regression2 = sm.OLS(y_train, x_train_2).fit()
print(regression2.summary())

# Verificare ipoteze

# Erorile reziduale sunt normal distribuite
sb.distplot(regression2.resid, fit=stats.norm)
plt.suptitle("Distributia reziduurilor")
plt.show()

# Exista o relatie liniara intre X si Y
sb.pairplot(data=tabel_1, vars=('atemp', 'cnt', 'hum', 'windspeed', 'temp'), diag_kind='kde')
plt.show()
# Utilizand acest grafic se poate observa ca exista o relatie liniara intre var "temp" si "atemp" cu var. predictoare "cnt"

# Testarea modelului
col = x_new.columns
x_test = x_test[col]
x_test_1 = sm.add_constant(x_test)
y_pred = regression2.predict(x_test_1)

fig = plt.figure()
plt.scatter(y_test, y_pred, alpha=.5)
fig.suptitle('y_test vs y_pred', fontsize=20)
plt.xlabel('y_test', fontsize=18)
plt.ylabel('y_pred', fontsize=16)
plt.show()

# Coeficinet R-patrat
from sklearn.metrics import r2_score

r2 = r2_score(y_test, y_pred)
print("Valoarea lui R^2 este: " + str(r2))
