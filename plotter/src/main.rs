use plotters::prelude::*;

#[derive(Debug, serde::Deserialize)]
pub struct Record {
    n: f32,
    fused: f32,
    classic: f32,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rdr = csv::Reader::from_reader(std::io::stdin());
    let mut vec_f: Vec<(f32, f32)> = Vec::new();
    let mut vec_c: Vec<(f32, f32)> = Vec::new();
    for result in rdr.deserialize() {
        let record: Record = result?;
        vec_c.push((record.n, record.classic));
        vec_f.push((record.n, record.fused));
    }

    let root_area = BitMapBackend::new("images/0.5.png", (600, 400)).into_drawing_area();
    root_area.fill(&WHITE).unwrap();
    let root_area = root_area.margin(10, 10, 10, 25);

    let mut ctx = ChartBuilder::on(&root_area)
        .set_label_area_size(LabelAreaPosition::Left, 40)
        .set_label_area_size(LabelAreaPosition::Bottom, 40)
        .caption("Church Numerals speed", ("sans-serif", 30))
        .build_cartesian_2d(0f32..10000f32, 0f32..10000f32)
        .unwrap();

    ctx.configure_mesh().draw().unwrap();

    ctx.draw_series(PointSeries::of_element(vec_c, 5, &RED, &|c, s, st| {
        return EmptyElement::at(c)    // We want to construct a composed element on-the-fly
            + Circle::new((0,0),s,st.filled());
    }))?;

    ctx.draw_series(PointSeries::of_element(vec_f, 5, &BLUE, &|c, s, st| {
        return EmptyElement::at(c)    // We want to construct a composed element on-the-fly
            + Circle::new((0,0),s,st.filled()); // At this point, the new pixel coordinate is established
    }))?;

    root_area.present()?;
    Ok(())
}
