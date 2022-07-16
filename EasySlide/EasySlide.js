const SLIDES = document.getElementsByClassName('slide');
let currentPage = 0;

// RESIZE
let changeSize = (e) => {
  let w = window.innerWidth;
  let h = window.innerHeight;
  Array.prototype.forEach.call(SLIDES, (elt) => {
    elt.style.width = w + 'px';
    elt.style.minWidth = w + 'px';
    elt.style.height = h + 'px';
    elt.style.minHeight = h + 'px';
    console.log(h, w);
  });
}
window.addEventListener('resize', changeSize);
window.addEventListener('load', changeSize);

// MOVE PAGES
let movePage = (diff) => {
  let target = currentPage + diff;
  if (target < 0){ target = 0; }
  if (target >= SLIDES.length){ target = SLIDES.length - 1; }
  let leftPos = window.innerWidth * target;
  console.log(leftPos);
  document.getElementById('main').scrollLeft =  leftPos;
  currentPage = target;
}

let getPushedKey = (e) => {
  console.log(e);
  if (e.keyCode === 39)  {// right
    movePage(1);
  } else if (e.keyCode === 37)  {// left
    movePage(-1);
  }
};
document.addEventListener('keydown', getPushedKey);
