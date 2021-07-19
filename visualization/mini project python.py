# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:percent
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.11.3
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# Ideas that we might want to explore: 
# movies_df.corr(): create a csv file contains vaccines, education, poverty and population and see correlation between them. 
#

# %%
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# %%
Vaccines=pd.read_csv('https://data.cdc.gov/api/views/8xkx-amqh/rows.csv')
Vaccines.head()

# %%
#Lets rename the columns
New_Column_Names={'MMWR_week':'Week',
                 'Recip_County':'County',
                 'Recip_State':'State',
                 'Series_Complete_Yes':'Completed_Pop',
                 'Series_Complete_Pop_Pct':'Completed_Pct',       
                 'Series_Complete_12PlusPop_Pct':'Completed_Pct_12plus',
                 'Series_Complete_12Plus':'Completed_Pop_12plus',
                 'Series_Complete_18PlusPop_Pct':'Completed_Pct_18plus',
                 'Series_Complete_18Plus':'Completed_Pop_18plus',
                 'Series_Complete_65PlusPop_Pct':'Completed_Pct_65plus',
                 'Series_Complete_65Plus':'Completed_Pop_65plus',
                 'Completeness_pct':'Complete_Info',
                  'Administered_Dose1_Pop_Pct':'Dose1_Pct',
                  'Administered_Dose1_Recip':'Dose1_Pop',
                  'Administered_Dose1_Recip_12PlusPop_Pct':'Dose1_Pct_12plus',
                  'Administered_Dose1_Recip12Plus':'Dose1_Pop_12plus',
                  'Administered_Dose1_Recip_18PlusPop_Pct':'Dose1_Pct_18plus',
                  'Administered_Dose1_Recip18Plus':'Dose1_Pop_18plus',                  
                  'Administered_Dose1_Recip_65PlusPop_Pct':'Dose1_Pct_65plus',
                  'Administered_Dose1_Recip65Plus':'Dose1_Pop_65plus'}
Vaccines.rename(New_Column_Names,axis=1,inplace=True)
Vaccines.Date=pd.to_datetime(Vaccines.Date)
Vaccines.head()


# %%
def fix_fips(x): 
    x=str(x)
    if x.isnumeric():
        return x.zfill(5)
    else:
        return x
Vaccines['FIPS']=Vaccines['FIPS'].apply(fix_fips)

# %%
#Data Frame with only the latest measurement for each County (FIPS)
Vaccines_Latest=Vaccines.groupby('FIPS').apply(lambda x: x.sort_values(by='Date',ascending=False).head(1))
Vaccines_Latest=Vaccines_Latest.reset_index(drop=True)

#We drop the "UNKNOWN" counties and the counties with zero vaccinations (probably they don't share their data)
Vaccines_Latest=Vaccines_Latest[Vaccines_Latest.FIPS!='UNK']
Vaccines_Latest=Vaccines_Latest[Vaccines_Latest.Completed_Pop>0]

# %%

# %%
print(Vaccines_Latest.shape)
Vaccines_Latest.head()

# %%
# Percent of people 18+ who are fully vaccinated in each county
Vaccines_Latest.plot(y='Completed_Pct_18plus', kind='hist',edgecolor='white')
Vaccines_Latest.FIPS.astype(object)

# %%
# !pip install plotly==5.1.0

# %%
from urllib.request import urlopen
import json
with urlopen('https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json') as response:
    counties = json.load(response)
import plotly.express as px

# %%
fig = px.choropleth(Vaccines_Latest, geojson=counties, locations='FIPS', color='Completed_Pct_18plus',
                           color_continuous_scale="RdBu_r",
                           range_color=(0, 100),
                           scope="usa",
                           hover_data={'FIPS':False,'County': True,'State': True, 'Dose1_Pct_18plus':True},
                           labels={'Completed_Pct_18plus':'% Completed (18+)', 'Dose1_Pct_18plus': '%1st Dose (18+)'}
                          )
fig.update_layout(margin={"r":0,"t":0,"l":0,"b":0})
fig.show()

# %%
# Estimated percent of people of all ages in poverty 2019
poverty=pd.read_csv('Poverty_Estimates.csv')
poverty.tail()

# %%
print(poverty.shape)


# %%
#We drop the "UNKNOWN" counties and the counties with zero percentage poverty (probably they don't share their data)
#poverty_Latest=poverty[poverty.FIPS!=0]
#poverty_Latest=poverty[poverty.PCTPOVALL_2019>0]
#print(poverty_Latest.shape)

# %%
#Lets rename the columns
New_Column_Names={'FIPStxt':'FIPS',
                 'Stabr':'State',
                 'Area_name':'County'}
poverty.rename(New_Column_Names,axis=1,inplace=True)
#Vaccines.Date=pd.to_datetime(Vaccines.Date)
poverty.head()
#poverty.FIPS.astype(object)

# %%
figpov = px.choropleth(poverty, geojson=counties, locations='FIPS', color='PCTPOVALL_2019',
                           color_continuous_scale="RdBu_r",
                           range_color=(0, 50),
                           scope="usa",
                           hover_data={'FIPS':False,'County': True,'State': True, 'PCTPOVALL_2019':True},
                           labels={'PCTPOVALL_2019':'% poverty (all ages)'}
                          )
figpov.update_layout(margin={"r":0,"t":0,"l":0,"b":0})
figpov.show()

# %%
# Estimated percent of people in all ages in poverty (2019)
poverty.plot(y='PCTPOVALL_2019', kind='hist',edgecolor='white')

# %%
poverty.boxplot(column='PCTPOVALL_2019', by='State');

# %%
poverty.FIPS=poverty.FIPS.apply(fix_fips)
Vaccines_poverty=Vaccines_Latest.merge(poverty,how='inner', on=['FIPS', 'State', 'County'])
Vaccines_poverty.shape
Vaccines_poverty.head()


# %%
import statsmodels.api as sm

# %%
lm = sm.OLS.from_formula('Completed_Pct_18plus ~ PCTPOVALL_2019', Vaccines_poverty)
result = lm.fit()
result.summary()


# %%
import seaborn as sns
sns.regplot('PCTPOVALL_2019', 'Completed_Pct_18plus', Vaccines_poverty, line_kws = {"color":"r"}, ci=None)


# %%
Education=pd.read_csv('https://www.ers.usda.gov/webdocs/DataFiles/48747/Education.csv',encoding="ISO-8859-1")
#Education=pd.read_csv('Education.csv',encoding="ISO-8859-1")
New_Column_Names={'FIPS Code':'FIPS',
                   'Percent of adults with less than a high school diploma, 2015-19':'Less_HighSchool',
                   'Percent of adults with a high school diploma only, 2015-19':'Only_HighSchool',
                   "Percent of adults completing some college or associate's degree, 2015-19":'College',
                   "Percent of adults with a bachelor's degree or higher, 2015-19":'College_Graduate'}
Education.rename(New_Column_Names,axis=1,inplace=True)
Education=Education[['FIPS', 'Less_HighSchool', 'Only_HighSchool', 'College', 'College_Graduate']]
Education.head()

# %%
Education.FIPS=Education.FIPS.apply(fix_fips)
Vaccines_Education=Vaccines_Latest.merge(Education,how='inner', on='FIPS')
Vaccines_Education.shape

# %%
Education.plot(kind='hist',subplots=True,density=True)

# %%
from sklearn.linear_model import Lasso
from sklearn.model_selection import train_test_split

indep_variables=['Less_HighSchool', 'Only_HighSchool','College','College_Graduate']
dep_variable=['Completed_Pct_18plus']

X=Vaccines_Education[indep_variables]
y=Vaccines_Education[[dep_variable[0]]]

# test train split
X_train,X_test,y_train,y_test = train_test_split(X, y,
                                                 test_size = .25,
                                                 random_state = 614,
                                                 shuffle = True)
X_train.shape, X_test.shape

# %%
from sklearn.metrics import mean_squared_error, r2_score

# %%
# Cross Validation
from sklearn.model_selection import KFold
kfold = KFold(n_splits=5,shuffle = True,random_state=440)

#We'll look at different alpha values
alphas=10**np.linspace(-2,2,20)

R2s=np.zeros([5,20])
MSEs=np.zeros([5,20])

# %%
for i, alpha in enumerate(alphas):
    j=0
    for train_index,test_index in kfold.split(X_train,y_train):
        X_train_train, X_train_test = X_train.iloc[train_index],X_train.iloc[test_index]
        y_train_train, y_train_test = y_train.iloc[train_index],y_train.iloc[test_index]

        lasso_model=Lasso(alpha=alpha,max_iter=10000)
        lasso_model.fit(X_train_train,y_train_train)
        
        y_predict = lasso_model.predict(X_train_test)
        mse=mean_squared_error(y_train_test,y_predict)
        r2=r2_score(y_train_test,y_predict)
        
        R2s[j,i]=r2
        MSEs[j,i]=mse
        
        j+=1   

# %%
MSEs.mean(axis=0)

# %%
#Choose the best model (with minimum MSE)
k=np.argmin(MSEs.mean(axis=0))
print('Min MSE: ',min(MSEs.mean(axis=0)))
print('Corresponding R2: ', R2s.mean(axis=0)[k])

# %%
#Repeat the model for the whole training set with this parameter. 
alpha=alphas[k]
lasso_model=Lasso(alpha=alpha, max_iter=10000)
lasso_model.fit(X_train,y_train)
print(lasso_model.coef_)

#Compute its score on the test set
y_predict=lasso_model.predict(X_test)

mse=mean_squared_error(y_test,y_predict)
r2=r2_score(y_test,y_predict)

print('MSE: ', mse)
print('R2: ', r2)

# %%
#This shows that only the last 3 leves of education have an impact.
#So we compute the linear model with the package sm
import statsmodels.api as sm
lm= sm.OLS.from_formula('Completed_Pct_18plus ~ Only_HighSchool+College + College_Graduate', Vaccines_Education)
result = lm.fit()
result.summary()

# %%
#From the previous chart, we can't refuse the null hypothesis that the variable Only_Highschool
#has a 0 coefficient (with 95% confidence). So we create a model using only the other two Variables. 
lm= sm.OLS.from_formula('Completed_Pct_18plus ~ College_Graduate', Vaccines_Education)
result = lm.fit()
result.summary()

# %%
import seaborn as sns
sns.regplot('College_Graduate', 'Completed_Pct_18plus', Vaccines_Education, line_kws = {"color":"r"}, ci=None)

# %%
Vaccines_poverty_Education=Vaccines_poverty.merge(Education,how='inner', on=['FIPS', 'State', 'County'])
Vaccines_poverty_Education.shape
Vaccines_poverty_Education.head()

# %%
lm1 = sm.OLS.from_formula('Completed_Pct_18plus ~ PCTPOVALL_2019 + College_Graduate ', Vaccines_poverty_Education)
result1 = lm1.fit()
result1.summary()

# %%
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import linear_model
from mpl_toolkits.mplot3d import Axes3D

######################################## Data preparation #########################################
 
X = Vaccines_poverty_Education[['PCTPOVALL_2019', 'College_Graduate']].values.reshape(-1,2)
Y = Vaccines_poverty_Education['Completed_Pct_18plus']

######################## Prepare model data point for visualization ###############################

x = X[:, 0]
y = X[:, 1]
z = Y

x_pred = np.linspace(0, 50, 30)   # range of percentage of poverty values
y_pred = np.linspace(0, 50, 30)  # range of percentage of education values
xx_pred, yy_pred = np.meshgrid(x_pred, y_pred)
model_viz = np.array([xx_pred.flatten(), yy_pred.flatten()]).T

################################################ Train #############################################

ols = linear_model.LinearRegression()
model = ols.fit(X, Y)
predicted = model.predict(model_viz)

############################################## Evaluate ############################################

r2 = model.score(X, Y)

############################################## Plot ################################################

# %%
# %matplotlib notebook

plt.style.use('default')

fig = plt.figure(figsize=(12, 4))

ax1 = fig.add_subplot(131, projection='3d')
#ax2 = fig.add_subplot(132, projection='3d')
#ax3 = fig.add_subplot(133, projection='3d')

axes = [ax1, ax2, ax3]

for ax in axes:
    ax.plot(x, y, z, color='k', zorder=15, linestyle='none', marker='o', alpha=0.1)
    ax.scatter(xx_pred.flatten(), yy_pred.flatten(), predicted, facecolor=(0,0,0,0), s=20, edgecolor='#70b3f0')
    ax.set_xlabel('Poverty (%)', fontsize=12)
    ax.set_ylabel('College graduate (%)', fontsize=12)
    ax.set_zlabel('Vaccinated above 18 (%)', fontsize=12)
    ax.locator_params(nbins=4, axis='x')
    ax.locator_params(nbins=5, axis='x')

ax1.text2D(0.2, 0.32, 'aegis4048.github.io', fontsize=13, ha='center', va='center',
           transform=ax1.transAxes, color='grey', alpha=0.2)
#ax2.text2D(0.3, 0.42, 'aegis4048.github.io', fontsize=13, ha='center', va='center',
         #  transform=ax2.transAxes, color='grey', alpha=0.5)
#ax3.text2D(0.85, 0.85, 'aegis4048.github.io', fontsize=13, ha='center', va='center',
#           transform=ax3.transAxes, color='grey', alpha=0.5)

ax1.view_init(elev=28, azim=120)
#ax2.view_init(elev=4, azim=114)
#ax3.view_init(elev=60, azim=165)

fig.suptitle('$R^2 = %.2f$' % r2, fontsize=20)

fig.tight_layout()

# %%
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt
import plotly.express as px
# %matplotlib inline
sns.set_style('darkgrid')
matplotlib.rcParams['font.size'] = 14
matplotlib.rcParams['figure.figsize'] = (9, 6)
matplotlib.rcParams['figure.facecolor'] = '#00000000'
plt.rc('font', size=12)

# %%

cols = ['State', 'Completed_Pct_18plus', 'County', 'FIPS']
vacc_amount = Vaccines_Latest[cols].groupby('State').max().sort_values('Completed_Pct_18plus', ascending=False).dropna(subset=['Completed_Pct_18plus'])
vacc_amount = vacc_amount.iloc[:52]

plt.figure(figsize=(16, 7))
plt.bar(vacc_amount.index, vacc_amount.Completed_Pct_18plus, color = 'c')

plt.title('Percentage of vaccinated people per state')
plt.xticks(rotation = 90)
plt.ylabel('Percentage of vaccinated people above 18')
plt.xlabel('States')
plt.show();

# %%
# Importing data for population
Population_complete = pd.read_csv('PopulationEstimates.csv')

#Renaming columns
New_Column_Names={'Area_Name' : 'County_x',
                 'POP_ESTIMATE_2019': 'Population' , 'State': 'State_x'}
Population_complete.rename(New_Column_Names, axis = 1, inplace = True)

# Keeping information about the 2019 Census
Population = Population_complete[['County_x', 'State_x', 'Population']]

# %%
#Adding Population to the previous data frame
Vac = Vaccines_Latest[['County', 'State', 'Completed_Pct_18plus']]
Vaccines_population = pd.merge(Vac, Population, on = ['County', 'State'])
Vaccines_population['Population'] = pd.to_numeric(Vaccines_population['Population'])


# %%
# Histogram of population
Vaccines_population.plot(y='Population', kind='density')

# Scatter of Population vs Vaccination percentage
Vaccines_population.plot(x = 'Population', y = 'Completed_Pct_18plus', kind = 'scatter', alpha = 0.5)

# %%
Vaccines_poverty_Education_population=Vaccines_poverty_Education.merge(Education,how='inner', on=['state_x','county_x'])
Vaccines_poverty_Education.shape
Vaccines_poverty_Education.head()
