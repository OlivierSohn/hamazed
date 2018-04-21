
function show_overlay(e){
  e.childNodes[0].style.display="block"
}
function hide_overlay(e){
  e.childNodes[0].style.display="none"
}
function toggle_details(e,evt){
  toggleExpand(e,e.childNodes[2],evt)
}
// using https://stackoverflow.com/a/26476282 for height animation:
const
toggleExpand = function(ownerElementAbove,element,evt) {
  var h = Array.prototype.reduce.call(element.childNodes, function(p, c) {return p + (c.offsetHeight || 0);}, 0);
  var ms = Math.min(1000,Math.ceil(40*Math.sqrt(h)));
  if (!element.style.height || element.style.height == '0px') {
      element.style.transitionDuration = ms.toString() + 'ms'
      element.style.height = h + 'px';
  } else {
      mouseY = evt?evt.clientY:(document.documentElement.clientHeight/2)
      var ownerTop = getCoordsWrtDoc(ownerElementAbove).top
      if(ownerTop < 20 + pageYOffset) {
          // top is not clearly visible (we use a 20 margin because of the sticky element)
          var target = (ownerTop + getCoordsWrtDoc(element).top) / 2
          scrollTo(target - mouseY, ms);
      }
      element.style.height = '0px';
  }
}
const
getCoordsWrtDoc = function(elem) {
  let box = elem.getBoundingClientRect();
  return {
    top: box.top + pageYOffset,
    left: box.left + pageXOffset
  };
}
const
getCoordsWrtScreen = function(elem) {
  return elem.getBoundingClientRect();
}
const
scrollTo = function(to, duration) {
    const
    element = document.scrollingElement || document.documentElement,
    start = element.scrollTop,
    change = to - start,
    startDate = +new Date(),
    // t = current progress
    // b = start value
    // c = change in value
    // d = duration
    easeLin = function(t, b, c, d) {
        return b + c * t / d;
    },
    animateScroll = function() {
        const currentDate = +new Date();
        const progress = currentDate - startDate;
        element.scrollTop = parseInt(easeLin(progress, start, change, duration));
        if(progress < duration) {
            requestAnimationFrame(animateScroll);
        }
        else {
            element.scrollTop = to;
        }
    };
    animateScroll();
};
