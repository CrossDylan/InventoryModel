from cutstock_util import getStockLength

def createWidthDemand(singleInputData):
#    fout1 = open('InputData.csv', 'r')
#    fout2 = open('WidthDemand.csv', 'w')
    widthDemand = []
#    fout1.readline()
    for eachline in singleInputData:
        q=eachline.split()[0].split(',')[1]
        widthDemand.append(q)
#        fout2.write(q+'\n')
#    fout1.close()
#    fout2.close()
    return widthDemand

def gen_patterns(stocklen, waste, pattlens):
##'Generates cutting stock patterns from the supplied stock length,' 
##'cut waste, and desired lengths supplied.'
    pattlen1 = pattlens[0] # The first length
    remlens = pattlens[1:] # The remaining lengths
    if remlens:
    # If the remaining lengths list is not empty:
        for i in reversed(range(1 + int((stocklen + waste) / (pattlen1 + waste)))):
            for p in gen_patterns(stocklen - i * (pattlen1 + waste), waste, remlens):
                yield (pattlen1,) * i + p
    else:
        # Stuff the number of pattlen1 we can cut into the pattern
        yield (pattlen1,) * int((stocklen + waste) / (pattlen1 + waste))

def makeWidthDict(plset):
    ws=dict()
    i=1
    for width in plset:
        ws[width]='w'+str(i)
        i+=1
    return ws

def createPatternsAndWaste(singleInputData,mill):
    # Read the input file:
#    fn='InputData.csv'
    #tf='test0ab.txt'
    #pattern = map(int, file(tf).read().split())
#    pattern = map(float,file(fn).read().replace('\n',',').split(',')[0:-1]) 
#    fout3 = open('Patterns.csv', 'w')
#    fout4 = open('Waste.csv', 'w')    
    patterns = []
    waste = []
    sl = getStockLength(mill) ##pattern[0] # Stock length
    w = 0 ## pattern[1] # Cut waste (paper lost from cutting, in our case none. ex. 1/8" from cutting wood)
#    pl = list(reversed(sorted(pattern[0:len(pattern):2]))) # The desired lengths
    pl = [float(x.split(',')[0]) for x in singleInputData]
    plset=set(pl)
    ws = makeWidthDict(plset)

    i = 0
    for p in gen_patterns(sl, w, pl):
        pwidth = 0
        if p:
            i += 1
        pset = set(p)
        for sku in pset:
            patterns.append('p%d, %s' % (i, ' '.join(map(str,[ws[sku],',',p.count(sku)]))))
            pwidth+= sku*p.count(sku)
        waste.append(sl-pwidth)
#    fout3.close()
#    fout4.close()
    return [patterns, waste]
