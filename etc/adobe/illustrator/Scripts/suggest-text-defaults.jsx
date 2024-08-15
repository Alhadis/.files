(function(){
	if(documents.length){
		var alignments = {left: 0, centre: 0, right: 0};
		var fontSizes = {};
		var fontFaces = {};
		var textFrames = activeDocument.textFrames;
		for(var i = 0; i < textFrames.length; ++i){
			var paragraphs = textFrames[i].paragraphs;
			for(var p = 0; p < paragraphs.length; ++p){
				var range = paragraphs[p];
				
				// Alignment mode
				var align = range.paragraphAttributes.justification;
				switch(align){
					case Justification.CENTER: ++alignments.centre; break;
					case Justification.LEFT:   ++alignments.left;   break;
					case Justification.RIGHT:  ++alignments.right;  break;
				}
				
				// Point-size(s) and typeface(s)
				var chars = range.characters;
				for(var c = 0; c < chars.length; ++c){
					var attr = chars[c].characterAttributes;
					var font = attr.textFont.name;
					var size = attr.size + "px";
					if(!(font in fontFaces)) fontFaces[font] = 0;
					if(!(size in fontSizes)) fontSizes[size] = 0;
					++fontFaces[font];
					++fontSizes[size];
				}
			}
		}
		
		// Prepare report in readable/parseable INI format
		alert([
			"Summary of text properties",
			"[Alignments]", describe(alignments),
			"[Fonts]",      describe(fontFaces, true),
			"[Sizes]",      describe(fontSizes),
		].join("\n").replace(/^\s+|\s+$/g, ""));
	}
	
	/**
	 * Format an object's enumerable properties as an INI list.
	 * @param {Object} obj - Subject being enumerated
	 * @param {Boolean} keepCase - Don't decamelise each property name
	 * @return {String}
	 */
	function describe(obj, keepCase){
		var output = "";
		for(var key in obj){
			var value = obj[key];
			if(!keepCase){
				key = key.replace(/(?!^)[A-Z]/g, " $&");
				key = key[0].toUpperCase() + key.slice(1);
			}
			output += key + " = " + value + "\n";
		}
		return output;
	}
})();
