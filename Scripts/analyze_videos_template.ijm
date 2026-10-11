//setBatchMode(true);
args = getArgument();
parts = split(args, ",");
video = parts[0];
outDir = parts[1];

print("Video: " + video);
print("Saving results to: " + outDir);

print("Processing video: " + video);        
open(video);
selectWindow(File.getName(video));
roiManager("Reset");
makeOval(0, 0, 768, 540);
roiManager("Add");
roiManager("Select", roiManager("Count") - 1);
roiManager("Rename", "Ventricle");
roiManager("Update");
roiManager("Select All");
roiManager("Multi Measure");

baseName = File.getNameWithoutExtension(video);
savePath = outDir + File.separator + baseName + ".csv";
saveAs("Results", savePath);
roiManager("Reset");
run("Close All");
//setBatchMode(false);


