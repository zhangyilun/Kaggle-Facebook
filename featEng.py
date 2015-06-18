__author__ = 'hujie'

import pandas as pd
import numpy as np
from sklearn import cross_validation
from sklearn.preprocessing import StandardScaler
from sklearn.utils import shuffle
import scipy.stats as stats
import random
from tqdm import tqdm
import gc

feat_size=26
#['bidder_id', 'auction', 'merchandise', 'device', 'time', 'country', 'ip', 'url']
# bidder_id,payment_account,address,outcome

random.seed()
def saveData(fpath,data,headers):
    df = pd.DataFrame(data)
    df.to_csv(fpath, header=headers,index=False)
    print('Data has been saved!')


def sortByTime(bid):
    return

def bidderFeatEng(bid_info):
    '''
    ['bidder_id', 'auction', 'merchandise', 'device', 'time', 'country', 'ip', 'url']
    return [

        top_merchandise_1,top_merchandise_2,top_merchandise_3,unique_merchandise_count,
        top_device_1,top_device_2,top_device_3,unique_device_count,
        top_country_1,top_country_2,top_country_3,unique_country_count,
        top_ip_1,top_ip_2,top_ip_3,unique_ip_count,
        top_url_1,top_url_2,top_url_3,unique_url_count,

        avg_time,var_time,zero_time,

        avg_bids_per_auction, var_bids_per_auction, total_bids,
    '''
    # Transform list of lists into a numpy array
    bid_info=np.array(bid_info)

    count_list=[2,3,5,6,7]
    rv=[]
    for i in range(len(count_list)):
        idx=count_list[i]
        data=stats.itemfreq(bid_info[:,idx])
        # Sort according to the count
        data=data[data[:, 1].argsort()]
        length=data.shape[0]
        rv.append(data[0,0])
        if length>=2:
            rv.append(data[1,0])
        else:
            rv.append(0)
        if length>=3:
            rv.append(data[2,0])
        else:
            rv.append(0)
        rv.append(length)

    t = bid_info[:, 4].astype(float)
    t_diff = t[1:] - t[:-1]
    avg_time=0
    var_time=0
    zero_time=0
    if t_diff.shape[0]>=1:
        avg_time, var_time = t_diff.mean(), t_diff.var()
        zero_time = t_diff.shape[0] - np.count_nonzero(t_diff)
    rv.extend([avg_time,var_time,zero_time])

    df = pd.DataFrame(bid_info[:, [0, 1]], columns=['bidder_id', 'auction'])
    bids_count = df.groupby(['auction']).count()['bidder_id'].as_matrix()
    avg_bids_per_auction, var_bids_per_auction= bids_count.mean(), bids_count.var()
    total_bids=len(bid_info)
    rv.extend([avg_bids_per_auction,var_bids_per_auction, total_bids])

    return np.array(rv)

def generateData(fname,bidders,test=False):
    data=pd.read_csv(fname)
    data=data.values

    data_x=np.zeros((len(data),feat_size))
    data_y=np.zeros(len(data))
    id=np.chararray(len(data),itemsize=37)
    for i in tqdm(range(len(data))):
        gc.collect()
        bidder_name=data[i][0]
        id[i]=bidder_name
        if bidder_name in bidders:
            bid_info=bidders[bidder_name]
            data_x[i,:]=bidderFeatEng(bid_info)
            '''
            for j in range(min(len(bid_info),feat_size)):
                idx = 7*j
                for k in range(7):
                    data_x[i,idx+k]=bid_info[j][k+1]
            '''
            if not test:
                data_y[i]=data[i][3]

    if not test:
        return data_x,data_y,id
    else:
        return data_x,id


def featEng():
    bidData=pd.read_csv("./data/bids.csv")
    bidData.set_index('bid_id',inplace = True)
    bid=bidData.values
    bidders={}

    # mappings
    map=[{},{},{},{},{},{},{},{}]

    for i in range(len(bid)):
        # Transforms data into integers
        for j in range(1,8):
            if bid[i][j] not in map[j]:
                if j!=4:
                    map[j][bid[i][j]]=len(map[j])+1
                else:
                    map[j][bid[i][j]]=bid[i][j]
            bid[i][j]=map[j][bid[i][j]]

        bidder_name=bid[i][0]
        if bidder_name not in bidders:
            bidders[bidder_name]=[]
        bidders[bidder_name].append(bid[i])

    for bidder_name in bidders:
        bidders[bidder_name] = sorted(bidders[bidder_name], key=lambda bid: bid[4])

    # Selects recent 100 bids for each bidder
    test_x,test_id=generateData('./data/test.csv',bidders,test=True)
    _train_x,_train_y,_train_id=generateData('./data/train.csv',bidders)


    _train_x,_train_y,_train_id=shuffle(_train_x,_train_y,_train_id)
    # 20% as validation data
    # valid data: 0~valid_size-1
    # train_size: ~len(_train_x)-1
    valid_size=0.2*len(_train_x)
    train_x=_train_x[valid_size+1:len(_train_x)-1,:]
    train_y=_train_y[valid_size+1:len(_train_x)-1]
    valid_x=_train_x[0:valid_size-1,:]
    valid_y=_train_y[0:valid_size-1]
    valid_id=_train_id[0:valid_size-1]

    scaler = StandardScaler()
    train_x = scaler.fit_transform(train_x)
    valid_x = scaler.transform(valid_x)
    test_x = scaler.transform(test_x)

    x_headers=[]
    for i in range(700):
        x_headers.append("feat_"+str(i))
    y_headers=["outcome"]

    saveData("./data/train_x.csv",train_x,x_headers)
    saveData("./data/train_y.csv",train_y,y_headers)
    saveData("./data/valid_x.csv",valid_x,x_headers)
    saveData("./data/valid_y.csv",valid_y,y_headers)
    saveData("./data/test_x.csv",test_x,x_headers)

    saveData("./data/valid_id.csv",valid_id,[])
    saveData("./data/test_id.csv",test_id,[])

featEng()