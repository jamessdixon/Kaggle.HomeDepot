import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor, BaggingRegressor
from nltk.stem.snowball import SnowballStemmer

stemmer = SnowballStemmer('english')

df_train = pd.read_csv('C:/Git/Kaggle.HomeDepot/ProjectSearchRelevance.Python/ProductSearchRelevance/input/train.csv', encoding="ISO-8859-1")
df_test = pd.read_csv('C:/Git/Kaggle.HomeDepot/ProjectSearchRelevance.Python/ProductSearchRelevance/input/test.csv', encoding="ISO-8859-1")
df_pro_desc = pd.read_csv('C:/Git/Kaggle.HomeDepot/ProjectSearchRelevance.Python/ProductSearchRelevance/input/product_descriptions.csv')

#Get number of rows in the train dataframe
num_train = df_train.shape[0]
df_train.head()

#functon that takes in a s
#splits s into words
#then makes all words lower
#iterate through all of the words
#and get tehm stem word
#return each word joined by a space
def str_stemmer(s):
	return " ".join([stemmer.stem(word) for word in s.lower().split()])

str_stemmer("angle bracket")

#given two words
#find how many times word 1 is in word 2
def str_common_word(str1, str2):
	return sum(int(str2.find(word)>=0) for word in str1.split())

#bring train and test together
#then bring in description
#for 1 massive data frame
df_all = pd.concat((df_train, df_test), axis=0, ignore_index=True)
df_all = pd.merge(df_all, df_pro_desc, how='left', on='product_uid')
df_all.describe()
df_all.head()

#stem all of the different fields
df_all['search_term'] = df_all['search_term'].map(lambda x:str_stemmer(x))
df_all['product_title'] = df_all['product_title'].map(lambda x:str_stemmer(x))
df_all['product_description'] = df_all['product_description'].map(lambda x:str_stemmer(x))


df_all['len_of_query'] = df_all['search_term'].map(lambda x:len(x.split())).astype(np.int64)
df_all['product_info'] = df_all['search_term']+"\t"+df_all['product_title']+"\t"+df_all['product_description']

df_all['word_in_title'] = df_all['product_info'].map(lambda x:str_common_word(x.split('\t')[0],x.split('\t')[1]))
df_all['word_in_description'] = df_all['product_info'].map(lambda x:str_common_word(x.split('\t')[0],x.split('\t')[2]))

df_all = df_all.drop(['search_term','product_title','product_description','product_info'],axis=1)

df_train = df_all.iloc[:num_train]
df_test = df_all.iloc[num_train:]
id_test = df_test['id']

y_train = df_train['relevance'].values
X_train = df_train.drop(['id','relevance'],axis=1).values
X_test = df_test.drop(['id','relevance'],axis=1).values

rf = RandomForestRegressor(n_estimators=15, max_depth=6, random_state=0)
clf = BaggingRegressor(rf, n_estimators=45, max_samples=0.1, random_state=25)
clf.fit(X_train, y_train)
y_pred = clf.predict(X_test)

#C:\Users\DIXON15\Documents\Python Scripts
pd.DataFrame({"id": id_test, "relevance": y_pred}).to_csv('pythonSubmission.csv',index=False)