package flo.Canvas;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.eclipse.swt.graphics.GC;

public class Drawer implements DrawingJob {

    private final Map<Integer, List<DrawingJob>> jobs;

    public Drawer() {
        jobs = new TreeMap<Integer, List<DrawingJob>>();
    }

    @Override
    public void draw(final GC gc) {
        for (final Entry<Integer, List<DrawingJob>> entry : jobs.entrySet())
            for (final DrawingJob job : entry.getValue())
                job.draw(gc);
    }

    public void addJob(final DrawingJob job, final int priority) {
        if (!jobs.containsKey(priority))
            jobs.put(priority, new ArrayList<DrawingJob>());
        jobs.get(priority).add(job);
    }
}
