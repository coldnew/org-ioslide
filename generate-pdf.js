/**
 * phantomjs script for printing presentations to PDF.
 *
 * Example:
 * phantomjs print-pdf.js "http://lab.hakim.se/reveal-js?print-pdf" reveal-demo.pdf
 *
 * This file is based on revealjs's print-pdf.js
 *
 * By Yen-Chin, Lee (https://github.com/coldnew)
 */

var page = require('webpage').create();
var system = require('system');

// Get url to render from command line.
var url;
if (system.args.length < 2) {
    url = 'http://coldnew.github.io/slides/org-ioslide';
    console.warn('Please specify url to generate pdf ' + url);
}
else {
    url = system.args[1];
}

console.log("Use url: " + url + " to generate pdf.");

var outputFile = system.args[2] || 'slides.pdf';

if( outputFile.match( /\.pdf$/gi ) === null ) {
    outputFile += '.pdf';
}

var slideWidth = system.args[3] ? system.args[3].split( 'x' )[0] : 960;
var slideHeight = system.args[3] ? system.args[3].split( 'x' )[1] : 700;

page.viewportSize = {
    width: slideWidth,
    height: slideHeight
};

// TODO
// Something is wrong with these config values. An input
// paper width of 1920px actually results in a 756px wide
// PDF.
page.paperSize = {
    width: Math.round( slideWidth * 2 ),
    height: Math.round( slideHeight * 2 ),
    border: 0
};

console.log( 'Printing PDF (Paper size: '+ page.paperSize.width + 'x' + page.paperSize.height +')' );

page.open( url, function( status ) {

    if (status !== 'success') {
        console.error('Failed to open inputfile ' + url);
        phantom.exit();
    }

    // Render all slides
    var next = function() {
        return page.evaluate(function() {
            return window.slidedeck.nextSlide();
        });
    };

    var isLastSlide = function() {
        return page.evaluate(function() {
            return (window.slidedeck.curSlide_ === window.slidedeck.slides.length - 1);
        });
    };

    var slideCount = page.evaluate(function() {
        return window.slidedeck.slides.length - 1;
    });

    var currentSlide = page.evaluate(function() {
        // FIXME: Why no use?
        return window.slidedeck.curSlide_;
    });

    function wait() {
        setTimeout(function() {
            phantom.exit();
        }, 5000);
    }

    var slideCounter = 1;
    while (true) {
        next();

        console.log("Slide count: " + slideCount);
        console.log("Current slide: " + currentSlide);

        // Capture.
        var filename = "aa" + slideCounter + '.png';
        console.log('Rendering ' + filename);
        page.render(filename);

        if (currentSlide == slideCount) break;
        next();
        slideCounter++;
        wait();
    }

    phantom.exit();

    // window.setTimeout( function() {
    //     console.log( 'Printed succesfully' );
    //     page.render( outputFile );
    //     phantom.exit();
    // }, 1000 );
} );
