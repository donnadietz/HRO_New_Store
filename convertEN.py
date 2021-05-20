#
f=open("EN.dat","r")
d=[]
for lyne in f:
    d.append(lyne)
f.close()

dic={}

for i in range(len(d)):
    temp=d[i].replace(","," ")
    temp=temp.split("|")
    dic[temp[4]]=(temp[22],temp[8],temp[9],temp[10],temp[11], temp[15],temp[16],temp[17],temp[18][0:5])

    
allCalls=list(dic)
f=open("EN.csv","w")
f.write("'call','FRN', 'first','init','last','suffix','address','city','state','zip'\n")
for call in allCalls:
    temp=str(dic[call])
    temp=temp[1:-1]
    f.write("'"+call+"', "+temp+"\n")

f.close()  
