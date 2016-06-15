# Goal: given a doc-topic composition file output by MALLET,
# get it into a tab-delimited structure for reading in by R

# Strategy: Read one line at a time: avoids loading everything into memory

tmloc="/Users/millerb/Documents/tm/"
dataset_name="realconsorts"
ntopics=100
destination=tmloc+dataset_name+"k"+`ntopics`+"_doc-all-topics.txt"
indexfile=tmloc+dataset_name+"k"+`ntopics`+"_composition.txt"


writer=open(destination, "w+")
with open(indexfile) as f:
    count=0
    for line in f:
        dict = {}
        if count %1000 ==0:print count
        count+=1
        if count>=1:
            a=line.split()
            
            # Use text number as ID; it's in the second column (0-indexed).
            # Don't forget to strip out the directories and the .txt
            Id=a[1]
            Id=Id.rsplit("_",1)[1]
            Id=Id.split(".",1)[0]
            
            # composition file now has topics as columns by default, 
            # so no need to zip as in reshapeMallet.py
            
            # first column: text ID
            writer.write(Id)
            
            # subsequent columns: topic proportions, in order
            for n in range(ntopics):
                try:
                    writer.write("\t"+str(a[n+2]))
                #To make sure there are no missing values
                except:
                    writer.write("\t"+str(0))
                    
            # Add line break
            writer.write("\n")
writer.close()
