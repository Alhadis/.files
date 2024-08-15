(function(doc){
	if(!doc) return;
	for(var i = 0; i < doc.layers.length; clear(doc.layers[i++]));
	app.redraw();
	function clear(subject){
		if(subject.name)
			subject.name = "";
		var items = subject.pageItems;
		if(items) for(var i = 0; i < items.length; clear(items[i++]));
		return subject;
	}
})(activeDocument);
