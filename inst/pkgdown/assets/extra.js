// Run when the document is fully loaded
$(document).ready(function() {
  // Code folding functionality
  addCodeFolding();
  
  // Add section numbering
  addSectionNumbers();
  
  // Number figures and tables
  numberFigures();
});

// Add code folding buttons to code blocks
function addCodeFolding() {
  // For each code block
  $('pre.sourceCode').each(function() {
    // Create the button
    var button = $('<button class="btn btn-sm code-folding-btn">Code</button>');
    
    // Insert button before code block
    $(this).before(button);
    
    // Wrap code block for styling
    $(this).wrap('<div class="code-wrapper"></div>');
    
    // Add click handler
    button.on('click', function() {
      $(this).toggleClass('active');
      $(this).next('pre.sourceCode').slideToggle();
    });
    
    // Initially hide code
    $(this).hide();
  });
}

// Add numbering to section headers
function addSectionNumbers() {
  var h2Counter = 0;
  var h3Counter = 0;
  
  // Process level 2 headings
  $('.section h2').each(function() {
    h2Counter++;
    h3Counter = 0;
    $(this).prepend('<span class="section-number">' + h2Counter + '. </span>');
  });
  
  // Process level 3 headings
  $('.section .section h3').each(function() {
    h3Counter++;
    $(this).prepend('<span class="section-number">' + h2Counter + '.' + h3Counter + ' </span>');
  });
}

// Number figures and tables
function numberFigures() {
  var figCounter = 0;
  
  // Process figures
  $('.figure').each(function() {
    figCounter++;
    var caption = $(this).find('p.caption');
    if (caption.length) {
      caption.prepend('<span class="figure-number">Figure ' + figCounter + ': </span>');
    }
  });
  
  // Reset counter for tables
  var tableCounter = 0;
  
  // Process tables with captions
  $('table.table').each(function() {
    var caption = $(this).find('caption');
    if (caption.length) {
      tableCounter++;
      caption.prepend('<span class="table-number">Table ' + tableCounter + ': </span>');
    }
  });
}