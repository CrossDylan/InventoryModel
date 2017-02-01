def getStockLength(mill):
    return {'SC':134,'KZ':141.875, 'BC':120,'MT':121.5, 'MC':194, 'WM':234, }[mill]

def getWasteList():
    raise ValueError("This function shouldn't be called with file io removed")
    wasteList=[]
    fout0 = open('Waste.csv', 'r')
    for eachline in fout0:
        wasteList.append(float(eachline.split()[0]))
    fout0.close()
    return wasteList

def getCutCount(widthDemand):
    # cutCount = 0
    # fout1 = open('WidthDemand.csv', 'r')
    # for eachline in fout1:
    #     cutCount += 1
    # fout1.close()
    # return cutCount
    return len(widthDemand)
    
def getPatCount(wasteList):
    # patCount = 0
    # fout2 = open('Waste.csv', 'r')
    # for eachline in fout2:
    #     patCount += 1
    # fout2.close()
    # return patCount
    return len(wasteList)

def getPriceSheetData():
    return 28
    
def getSheetsAvail():
    return 2000
    
def getCuts(cutcount):
#    cutcount = getCutCount(widthDemand)
    Cuts = range(cutcount)
    for i in range(cutcount):
        nstr = str(i+1)
        Cuts[i] = 'w' + nstr
    return Cuts

def getPatterns(patcount):
#    patcount = getPatCount()
    Patterns = range(patcount)
    for j in range(patcount):
        pstr = str(j+1)
        Patterns[j] = 'P' + pstr
    return Patterns 
    
def getCutDemand(widthDemand, cutcount):
    i = 0
#    cutcount = getCutCount()
    CutDemand = range(cutcount)
#    fout1 = open('WidthDemand.csv', 'r')
    for eachline in widthDemand:
        str = eachline.rstrip("\n")
        CutDemand[i] = float(str)
        i += 1
#    fout1.close()
    return CutDemand

def getCutsInPattern(patterns, cutcount, patcount):
#    cutcount = getCutCount()
#    patcount = getPatCount()
    CutsInPattern = [[0 for col in range(patcount)] for row in range(cutcount)]
#    fout2 = open('Patterns.csv', 'r')
    for eachline in patterns: # fout2.readlines():
        str = eachline
        lstr = str.split(",")
        pstr = lstr[0]
        wstr = lstr[1]
        cstr = lstr[2]
        pstr = pstr.replace("p","")
        wstr = wstr.replace("w","")
        cstr = cstr.rstrip("\n")
        p = int(pstr)
        w = int(wstr)
        c = int(cstr)
        CutsInPattern[w-1][p-1] = c
#    fout2.close()
    return CutsInPattern
