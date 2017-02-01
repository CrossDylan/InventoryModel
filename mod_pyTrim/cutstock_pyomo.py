from pyomo.core import *
import pyomo.opt
from pyomo.opt import SolverFactory
from pyomo.environ import ConcreteModel
from cutstock_util import*
from cutstock_input import *
from time import sleep

def runTrimModel(singleInputData, mill):
    # Generate Raw Data files from input sheet
    widthDemand = createWidthDemand(singleInputData)
    [patterns, wasteList] = createPatternsAndWaste(singleInputData,mill)

    # Reading in Data using the cutstock_util
    sl=getStockLength(mill)
#    wasteList = getWasteList()
    cutcount = getCutCount(widthDemand)
    patcount = getPatCount(wasteList)
    Cuts = getCuts(cutcount)
    Patterns = getPatterns(patcount)
    PriceSheet = getPriceSheetData()
    #SheetsAvail = getSheetsAvail()
    CutDemand = getCutDemand(widthDemand, cutcount)
    CutsInPattern = getCutsInPattern(patterns, cutcount, patcount)
    ########################################
    #CutsInPattern = makeDict([Cuts,Patterns],CutsInPattern)
    tmp = {}
    for i in range(len(Cuts)):
        tmp[Cuts[i]] = {}
        for j in range(len(CutsInPattern[i])):
            tmp[Cuts[i]][Patterns[j]] = CutsInPattern[i][j]
    CutsInPattern = tmp
    ########################################
    #CutDemand = makeDict([Cuts],CutDemand)
    tmp = {}
    for i in range(len(Cuts)):
        tmp[Cuts[i]] = CutDemand[i]
    CutDemand = tmp

    model = ConcreteModel(name="CutStock Problem")

    #Defining Variables
    model.SheetsCut = Var()
    model.TotalCost = Var()
    model.PatternCount = Var(Patterns, domain=NonNegativeIntegers, bounds=(0,None))
    model.ExcessCuts = Var(Cuts, bounds=(0,None))

    #objective
    model.objective = Objective(expr=1.0*model.TotalCost)

    #Constraints
    model.TotCost = Constraint(expr = model.TotalCost == model.SheetsCut) #PriceSheet* model.SheetsCut)
    #model.RawAvail = Constraint(expr = model.SheetsCut <= SheetsAvail)
    model.Sheets = Constraint(expr = summation(model.PatternCount) == model.SheetsCut)
    model.CutReq = Constraint(Cuts, noruleinit=True)
    for c in Cuts:
        model.CutReq.add(c, expr=sum(CutsInPattern[c][p]*model.PatternCount[p] 
                            for p in Patterns) == CutDemand[c] + model.ExcessCuts[c])

    instance = model #.create()
    solver='cbc' # cbc or glpk
    opt = pyomo.opt.SolverFactory(solver) 
    opt.options[{'cbc': 'seconds', 'glpk': 'tmlim'}[solver]]=30
    results = opt.solve(instance)
    ##results = opt.solve(model)
    instance.solutions.load_from(results)

#    return (results)
    # print "Status:", results.solver.status
    # print "Termination Condition:", results.solver[0]['Termination condition']
    # print "Minimum total cost:", value(instance.objective)
    
    trimWaste = 0
    sideRollWaste = 0
    cost = 0 
    for pat in model.PatternCount:
        v=model.PatternCount[pat].value
        if v >0:
            if wasteList[int(pat[1:])-1]<15:
                trimWaste+=wasteList[int(pat[1:])-1]*v
                if wasteList[int(pat[1:])-1]>=4:
                    cost+=wasteList[int(pat[1:])-1]*v*(332.17/17.5)
                else:
                    cost+=wasteList[int(pat[1:])-1]*v*(605/17.5)
            if wasteList[int(pat[1:])-1]>=15:
                sideRollWaste+=wasteList[int(pat[1:])-1]*v
                if wasteList[int(pat[1:])-1]<=24.5:
                    cost+=wasteList[int(pat[1:])-1]*v*(258.63/17.5)
                else:
                    cost+=wasteList[int(pat[1:])-1]*v*(196.29/17.5)
    trimWastePer = trimWaste/(value(instance.objective)*sl)        
    return [round(trimWaste,2), round(trimWastePer,3), round(sideRollWaste,2), round(cost,2)]






