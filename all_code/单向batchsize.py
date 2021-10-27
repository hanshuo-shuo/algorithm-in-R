#!/usr/bin/env python
import numpy as np
import pandas as pd
import torch
import matplotlib.pyplot as plt
from torch.autograd import Variable
import torch.utils.data as Data
import random
# split a univariate sequence into samples
def split_sequence(sequence, n_steps):
    X, y = [], []
    for i in range(len(sequence)):
        # find the end of this pattern
        end_idx = i + n_steps
        # check if we are beyond the sequence
        if end_idx > len(sequence) - 1:
            break
        # gather input and output parts if the pattern
        seq_x, seq_y = sequence[i:end_idx], sequence[end_idx]
        X.append(seq_x)
        y.append(seq_y)
    return np.array(X), np.array(y)
#!/usr/bin/env python
import torch
import torch.nn as nn
from torch.nn import functional as F
from torch import optim

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
data0 = pd.read_excel("2011-2020price.xlsx")
col_name = list(data0.columns)
data = data0[col_name[1:]]
index = data0[col_name[0]]

# plt.plot(data)
# plt.show()


dataset = data.dropna().values.astype('float32')
dataset = dataset.reshape(-1,1)

print(dataset.shape)


def centralize(data):
    min_value = np.min(data, axis=0)
    max_value = np.max(data, axis=0)
    data = (data - min_value) / (max_value - min_value)
    return data


def judge(dataset, k):
    '''
    to see at day k, if each stock rise or fall
    '''
    pr_today = dataset[k]
    pr_yesterday = dataset[k - 1]
    pr_change = pr_today - pr_yesterday
    med = np.median(pr_change)
    re = np.zeros_like(pr_change)
    re[pr_change > 0] = 1
    return (re)


def acc(out, y_real):
    out1 = np.zeros_like(out)
    out1[out > 0] = 1
    out1[out <= 0] = 0
    return 1 - sum(sum(abs(y_real - out1))) / (np.prod(y_real.shape))


def create_dataset(dataset, look_back=240):
    dataX, dataY = [], []
    for i in range(len(dataset) - look_back):
        pr_change = judge(dataset, i + look_back)
        a = dataset[i:(i + look_back)]
        dataX.append(a)
        dataY.append(pr_change)
    return np.array(dataX), np.array(dataY)


def set_seed(seed):
    torch.manual_seed(seed)  # cpu 为CPU设置种子用于生成随机数，以使得结果是确定的
    torch.cuda.manual_seed(seed)  # gpu 为当前GPU设置随机种子
    torch.backends.cudnn.deterministic = True  # cudnn
    np.random.seed(seed)  # numpy
    random.seed(seed)


look_back = 240
index_used = index[look_back:]
index_used = np.array(index_used)
X, Y = create_dataset(dataset, look_back)
print(X.shape, Y.shape)
a, b, c = X.shape

train_size = int(len(X) * 0.9)
valid_size = len(X) - train_size
index_size = int(len(index_used) * 0.9)
print(train_size, valid_size)

X_train = X[:train_size]
Y_train = Y[:train_size]
index_train = index_used[:index_size]

X_valid = X[train_size:]
Y_valid = Y[train_size:]
index_valid = index_used[index_size:]

# X_train = X_train.reshape(-1,198,240)
# X_valid = X_valid.reshape(-1,198,240)
# Y_train = Y_train.reshape(-1,198,1)

X_train = X_train.reshape(train_size * c, b, 1)
Y_train = Y_train.reshape(train_size * c, 1)
X_valid = X_valid.reshape(valid_size * c, b, 1)
Y_valid = Y_valid.reshape(valid_size * c, 1)

# X_train = X_train.transpose(1, 0, 2)
# X_valid = X_valid.transpose(1, 0, 2)

X_train = torch.from_numpy(X_train)
Y_train = torch.from_numpy(Y_train)
X_valid = torch.from_numpy(X_valid)

print(X_train.shape, Y_train.shape)


index_train = index[:index_size]
index_valid = index[int(len(index)*0.9):]

index_valid.shape
X.shape




class LSTMRegression(nn.Module):
    def __init__(self, input_size, hidden_size, output_size=1, num_layers=1):
        super().__init__()
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers, batch_first=True)
        self.linear = nn.Linear(hidden_size, output_size)

    def forward(self, x):
        _, (hn, cn) = self.lstm(x)
        hn = hn.squeeze()
        out = self.linear(hn)
        return out


model = LSTMRegression(input_size=1, hidden_size=5, output_size=1)

criterion = torch.nn.BCEWithLogitsLoss()  # 交叉熵BCEWithLogitsLoss()和MultiLabelSoftMarginLoss()
# criterion = torch.nn.CrossEntropyLoss()
optimizer = optim.Adam(model.parameters(), lr=1e-3)
# optimizer = optim.SGD(model.parameters(), lr=1e-1)

epochs =20
batch_size = 60
batch = X_train.shape[0] // batch_size

torch_dataset = Data.TensorDataset(torch.tensor(X_train), torch.tensor(Y_train))
# 把 dataset 放入 DataLoader
loader = Data.DataLoader(
    dataset=torch_dataset,  # torch TensorDataset format
    batch_size=batch_size,  # mini batch size
    shuffle=True,  #
    num_workers=10,  # 多线程来读数据
)

loss_epoch = np.zeros(epochs)
acc_epoch = np.zeros(epochs)
for epoch in range(epochs):
    loss_ep = np.array([])
    acc_ep = np.array([])
    for step, (var_x, var_y) in enumerate(loader):
        out = model(var_x)
        out_f = out.detach().clone().numpy()
        var_yf = var_y.detach().clone().numpy()
        loss = criterion(out, var_y)
        loss_f = loss.detach().clone().numpy()
        acc_ep = np.append(acc_ep, acc(out_f, var_yf))
        loss_ep = np.append(loss_ep, loss_f)

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

    if (epoch + 1) % 2 == 0:
        print(f'Epoch: {epoch:5d}, Loss: {np.mean(loss_ep):.4e}, Acc:{np.mean(acc_ep):.4e}')

    loss_epoch[epoch] = np.mean(loss_ep)
    acc_epoch[epoch] = np.mean(acc_ep)

# test
#X = X.reshape(-1,198,240)
#X = torch.from_numpy(X_valid)
Y_pred = model(X_valid)
Y_pred = Y_pred.clone().detach().numpy()
pred_acc = acc(Y_pred,Y_valid)
print(f'pred_acc{pred_acc}')
#Y_pred = Y_pred.view(-1).data.numpy()
# visulize
kind = 2
series = np.arange(kind*len(index_valid),(kind+1)*len(index_valid))
Y_pred_re = Y_pred
Y_pred_re[Y_pred_re>0] = 1
Y_pred_re[Y_pred_re<=0] = 0


fig = plt.figure()
ax = plt.subplot()
type1 = ax.scatter(index_valid, Y_valid[series], alpha=0.5,color='b',label='groundtruth')
type2 = ax.scatter(index_valid, Y_pred_re[series], alpha=0.5,color='r',label='prediction')
plt.xlabel("date time")
plt.ylabel("0 for fall, 1 for rise")
ax.legend((type1, type2), (u'groundtruth', u'prediction'), loc='best')
plt.show()

plt.plot(loss_epoch, 'r-', label='loss')
plt.plot(acc_epoch, 'b-', label='accurate rate')
plt.legend(loc='best')
plt.show()