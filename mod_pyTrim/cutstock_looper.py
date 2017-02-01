import cutstock_pyomo
import sys


pythonInputPath = sys.argv[1]
mill = sys.argv[2]
#trimData = sys.argv[3]    # Include if can find how to pass trim data as input instead of from csv

#pythonInputPath = "WM_output_for_trim.csv"
#mill = "WM" 

## Read in multiInputData

#data=[]
dataGroups={}
fout1 = open(pythonInputPath, 'r')
fout1.readline()   # clear header
for eachline in fout1:
    dat = eachline.split(',')     # parse to list
    grade = dat[1]
    caliper = dat[2]
    cycle = dat[6]+'-'+dat[7]
    gcc='['+grade+', '+caliper+', '+cycle+']'
    # gcc = str(map(int,dat[0:2]))             # create caliper, cycle key
    if int(dat[4])>0:
        try:
            dataGroups[gcc].append(dat)          # if caliper cycle exists
        except:                                  # if new cal cycle add it to dataGrups dict
            dataGroups[gcc]=[]
            dataGroups[gcc].append(dat)
fout1.close()

## For each group
#print('Running PM Trim Model')

# Create input data.csv with width and quantity
# Then run model

#foutf = open(pyTrimResults, "w") #Clear Output file
#foutf.truncate()
#foutf.write('Grade, Caliper, Cycle, Inches Trimmed, % Trimmed, Side Roll Waste, Cost\n')

print('Grade, Caliper, Cycle, Inches Trimmed, % Trimmed, Side Roll Waste, Cost')

once=1

trimResults = []
for gcc in dataGroups: #['[1, 1]']:
    if once ==1:
        #once+=1
        # fout2 = open('InputData.csv', 'w')
        singleInputData = []
        #        print(gcc)
        for width in dataGroups[gcc]:
#            print(int(width[7]))
            # fout2.write(width[3]+','+width[5]+'\n')
            singleInputData.append(width[3]+','+width[5])
        # fout2.close()
    # if (input('Continue: ')).lower()=='n':
    #     break
        a=cutstock_pyomo.runTrimModel(singleInputData,mill)
        b= gcc[1:-1]
        b = b.replace("\n",", ")
        print(b+str(a)[1:-1])
#        foutf.write(b+str(a)[1:-1]+'\n')
        
#foutf.close()


# Run cutstock_pyomo
# Calculate trim waste
# Append to trim output
