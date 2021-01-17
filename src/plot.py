#Open the output file and split the data to to a list
file = open("../run/output.dat","r")
data = file.read()
data = data.split(",")
data = map(lambda s: s.strip(), data)
data =list(data)
del data[-1]
#Make three lists
healthy = data[0::3]
sick = data[1::3]
immune = data[2::3]

healthy = list(map(float,healthy))
sick = list(map(float,sick))
immune = list(map(float,immune))

#Create the x-axis and make the values proportional to the population
time = range(0,len(sick))
population=(healthy[0]+sick[0]+immune[0])

healthy[:] = [x/population for x in healthy]
sick[:] = [x/population for x in sick]
immune[:] = [x/population for x in immune]



#Plot the values
import matplotlib.pyplot as plt

sick=plt.plot(time,sick,label="Sick")
immune=plt.plot(time,immune,label="Immune")
healthy=plt.plot(time,healthy,label="Healthy")


plt.title("Population Status")
plt.xlabel("timestep")
plt.ylabel("Population proportion")
#plt.xlim(0,1500)
plt.legend()
plt.show()
