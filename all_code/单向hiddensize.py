model = LSTMRegression(input_size=1, hidden_size=5, output_size=1)

criterion = torch.nn.BCEWithLogitsLoss()     #交叉熵BCEWithLogitsLoss()和MultiLabelSoftMarginLoss()
#criterion = torch.nn.CrossEntropyLoss()
optimizer = optim.Adam(model.parameters(), lr=1e-3)
#optimizer = optim.SGD(model.parameters(), lr=1e-1)

epochs = 100
batch_size = 30
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
    
    if (epoch + 1) % 5 == 0:
        print(f'Epoch: {epoch:5d}, Loss: {np.mean(loss_ep):.4e}, Acc:{np.mean(acc_ep):.4e}')


    loss_epoch[epoch] = np.mean(loss_ep)
    acc_epoch[epoch] = np.mean(acc_ep)

# test
#X = X.reshape(-1,198,240)
#X = torch.from_numpy(X_valid)
Y_pred = model(X_valid)
Y_pred = Y_pred.clone().detach().numpy()
pred_acc = acc(Y_pred,Y_valid)
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

model = LSTMRegression(input_size=1, hidden_size=2, output_size=1)

criterion = torch.nn.BCEWithLogitsLoss()     
optimizer = optim.Adam(model.parameters(), lr=1e-3)

epochs = 100
batch_size = 30
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
    
    if (epoch + 1) % 5 == 0:
        print(f'Epoch: {epoch:5d}, Loss: {np.mean(loss_ep):.4e}, Acc:{np.mean(acc_ep):.4e}')


    loss_epoch[epoch] = np.mean(loss_ep)
    acc_epoch[epoch] = np.mean(acc_ep)

Y_pred = model(X_valid)
Y_pred = Y_pred.clone().detach().numpy()
pred_acc = acc(Y_pred,Y_valid)

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

model = LSTMRegression(input_size=1, hidden_size=3, output_size=1)

criterion = torch.nn.BCEWithLogitsLoss()     
optimizer = optim.Adam(model.parameters(), lr=1e-3)

epochs = 100
batch_size = 30
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
    
    if (epoch + 1) % 5 == 0:
        print(f'Epoch: {epoch:5d}, Loss: {np.mean(loss_ep):.4e}, Acc:{np.mean(acc_ep):.4e}')


    loss_epoch[epoch] = np.mean(loss_ep)
    acc_epoch[epoch] = np.mean(acc_ep)

Y_pred = model(X_valid)
Y_pred = Y_pred.clone().detach().numpy()
pred_acc = acc(Y_pred,Y_valid)

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