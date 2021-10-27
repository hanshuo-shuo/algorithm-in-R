%%time
class LSTMRegression(nn.Module):
    def __init__(self, input_size, hidden_size, output_size=1, num_layers=2,bidirectional=True):
        super().__init__()
        self.lstm = nn.LSTM(input_size, hidden_size, num_layers,batch_first=True,bidirectional=True)
        self.linear = nn.Linear(2*hidden_size, output_size)
    
    def forward(self, x):
        x, _ = self.lstm(x) # (seq, batch, hidden)
        s, b, h = x.shape
#         print(s,b)
        x = x.contiguous().view(s*b, h) # 转换成线性层的输入格式
        x = self.linear(x)
        x = x.view(s, b, -1)
        return x



torch.manual_seed(7) #cpu
torch.cuda.manual_seed(7) #gpu
 
np.random.seed(7) #numpy
random.seed(7) # random and transforms
torch.backends.cudnn.deterministic=True #cudnn

model = LSTMRegression(input_size=240, hidden_size=4, output_size=1)

criterion = torch.nn.BCEWithLogitsLoss()     #交叉熵BCEWithLogitsLoss()和MultiLabelSoftMarginLoss()
#criterion = torch.nn.CrossEntropyLoss()
optimizer = optim.Adam(model.parameters(), lr=1e-3)
#optimizer = optim.SGD(model.parameters(), lr=1e-1)

epochs = 100
batch_size = 40
batch = X_train.shape[0] // batch_size

torch_dataset = Data.TensorDataset(torch.tensor(X_train), torch.tensor(Y_train))
# 把 dataset 放入 DataLoader
loader = Data.DataLoader(
    dataset=torch_dataset,  # torch TensorDataset format
    batch_size=batch_size,  # mini batch size
    shuffle=True,  #
    num_workers=2,  # 多线程来读数据
)

loss_epoch = np.zeros(epochs)
acc_epoch = np.zeros(epochs)
for epoch in range(epochs):
    acc_epo = 0
    loss_ep = np.array([])
    acc_ep = np.array([])
    for step,(var_x,var_y) in enumerate(loader):
        out = model(var_x)
        out_f = out.detach().clone().numpy()
        var_yf = var_y.detach().clone().numpy()
        loss = criterion(out, var_y)
        loss_f = loss.detach().clone().numpy()
        acc_ep = np.append(acc_ep,acc(out_f,var_yf))
        loss_ep = np.append(loss_ep,loss_f)

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        
    if (epoch + 1) % 10 == 0:
        #print(f'Epoch: {epoch:5d}, Loss: {loss.item():.4e}, Acc:{acc_epo/(X_train.shape[0]*X_train.shape[1]):.4e}')
        print(f'Epoch: {epoch:5d}, Loss: {np.mean(loss_ep):.4e}, ACC: {np.mean(acc_ep):.5e}')
        
    loss_epoch[epoch] = np.mean(loss_ep)
    acc_epoch[epoch] = np.mean(acc_ep)

# test
X_valid = X_valid.reshape(-1,198,240)
#X_valid = torch.from_numpy(X_valid)
Y_pred = model(X_valid)

kind = 3
Y_pred = torch.squeeze(Y_pred,2)
Y_pred = Y_pred.clone().detach().numpy()
pred_acc = acc(Y_pred,Y_valid)

Y_pred_re = Y_pred[:,kind]
Y_pred_re[Y_pred_re>0] = 1
Y_pred_re[Y_pred_re<=0] = 0
k = len(Y_pred_re)
series = np.arange(1,k,k//100)

fig = plt.figure()
ax = plt.subplot()
type1 = ax.scatter(index_valid[series], Y_valid[series,kind], alpha=0.5,color='b',label='groundtruth') 
type2 = ax.scatter(index_valid[series], Y_pred_re[series], alpha=0.3,color='r',label='prediction') 
plt.xlabel("date time")
plt.ylabel("0 for fall, 1 for rise")
ax.legend((type1, type2), (u'groundtruth', u'prediction'), loc='best')
plt.show()

plt.plot(loss_epoch, 'r-', label='loss')
plt.plot(acc_epoch, 'b-', label='accurate rate')
plt.legend(loc='best')
plt.show()