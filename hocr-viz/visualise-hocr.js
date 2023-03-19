$(function() {
    function parseTitle(text, key) {
        var parts = text.split("; ");
        for (var i = 0; i < parts.length; i++) {
            var words = parts[i].split(" ");
            if (words[0] == key) {
                if (key != "bbox") {
                    return words[1];
                }

                var left   = parseInt(words[1]);
                var top    = parseInt(words[2]);
                var right  = parseInt(words[3]);
                var bottom = parseInt(words[4]);
                return {
                    left:   Math.min(left, right),
                    top:    Math.min(top, bottom),
                    right:  Math.max(left, right),
                    bottom: Math.max(top, bottom),
                };
            }
        }
        return null;
    }

    function isBlock(css_class) {
        return [
            "ocr_page",
            "ocr_column",
            "ocr_carea",
            "ocr_float",
            "ocr_textfloat",
            "ocr_table",
            "ocr_document",
            "ocr_part",
            "ocr_chapter",
            "ocr_section",
            "ocr_subsection",
            "ocr_subsubsection",
            "ocr_par",
            "ocrx_block",
        ].includes(css_class);
    }

    function visualise() {
        var top_offset = 0;

        $(".ocr_page").each(function(index, element) {
            var ocr_page = $(this);
            var page_bbox = parseTitle(ocr_page.attr("title"), "bbox");
            var scale = $(window).width() / page_bbox.right;

            ocr_page.css("top", top_offset)
                    .height(scale * (page_bbox.bottom - page_bbox.top));

            ocr_page.find('[class*="ocr_"], [class*="ocrx_"]').each(function(index, element) {
                var ocr_element = $(this);

                element_bbox = parseTitle(ocr_element.attr("title"), "bbox");
                var elem_width = scale * (element_bbox.right - element_bbox.left);
                var elem_height = scale * (element_bbox.bottom - element_bbox.top);

                ocr_element.css("top", scale * element_bbox.top)
                           .css("left", scale * element_bbox.left)
                           .width(elem_width)
                           .height(elem_height);

                var angle = parseTitle(ocr_element.attr("title"), "textangle");
                if (angle == "90" || angle == "270") {
                    ocr_element.css("writing-mode", "vertical-rl")
                               .css("text-orientation", "sideways");
                }
                if (angle == "90" || angle == "180") {
                    // TODO: this requires position: absolute in order to work
                    // i.e. every element positioned relative to its parent
                    // ocr_element.css("transform", "rotate(0.5turn)");
                }

                if (isBlock(ocr_element.attr("class"))) {
                    return;
                }

                var font_size = Math.min(elem_width, 0.7 * elem_height);
                var short = (ocr_element[0].innerText.length <= 2);
                if (short) {
                    font_size = 0.7 * elem_height;
                }
                if (font_size < 1) {
                    ocr_element.css("font-size", "xx-small");
                } else {
                    ocr_element.css("font-size", font_size);
                }

                if (elem_height * 0.7 <= elem_width || short) {
                    ocr_element.css("padding-top", 0.05 * elem_height)
                               .css("word-break", "normal");
                } else {
                    ocr_element.css("text-align", "center")
                               .css("word-break", "break-word");
                }
            });

            top_offset += scale * page_bbox.bottom;
        });
    }

    $(window).resize(visualise);
    visualise();
});
