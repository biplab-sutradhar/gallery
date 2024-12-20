const dataPath = './data/visualizations.json';
const galleryBody = document.getElementById('gallery-body');

fetch(dataPath)
  .then(response => response.json())
  .then(data => {
    data.forEach(item => {
      const row = document.createElement('tr');
      const screenshotCell = document.createElement('td');
      const titleCell = document.createElement('td');
      const repoLinkCell = document.createElement('td');

      const img = document.createElement('img');
      img.src = item.screenshot;
      img.alt = item.title;
      screenshotCell.appendChild(img);

      const repoLink = document.createElement('a');
      repoLink.href = item.repoLink;
      repoLink.target = '_blank';
      repoLink.textContent = item.title;
      repoLinkCell.appendChild(repoLink);

      row.appendChild(screenshotCell);
      row.appendChild(repoLinkCell);
      galleryBody.appendChild(row);
    });
  })
  .catch(error => console.error('Error loading gallery:', error));
